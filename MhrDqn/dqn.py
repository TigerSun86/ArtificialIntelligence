import collections
import copy
import ctypes
import multiprocessing as mp
import multiprocessing.synchronize
import os
import time
import typing
from logging import Logger, getLogger
from typing import Any, Callable, Dict, List, Optional, Sequence, Tuple

import numpy as np
import torch
import torch.nn.functional as F

from abc import ABCMeta, abstractmethod, abstractproperty

import contexts
import action_value
import recurrent
import explorer
import replay_buffer2 as rb
import copy_param
import batch_states
import agent


class AttributeSavingMixin(object):
    """Mixin that provides save and load functionalities."""

    @abstractproperty
    def saved_attributes(self) -> Tuple[str, ...]:
        """Specify attribute names to save or load as a tuple of str."""
        pass

    def save(self, dirname: str) -> None:
        """Save internal states."""
        self.__save(dirname, [])

    def __save(self, dirname: str, ancestors: List[Any]):
        os.makedirs(dirname, exist_ok=True)
        ancestors.append(self)
        for attr in self.saved_attributes:
            assert hasattr(self, attr)
            attr_value = getattr(self, attr)
            if attr_value is None:
                continue
            if isinstance(attr_value, AttributeSavingMixin):
                assert not any(
                    attr_value is ancestor for ancestor in ancestors
                ), "Avoid an infinite loop"
                attr_value.__save(os.path.join(dirname, attr), ancestors)
            else:
                if isinstance(
                    attr_value,
                    (torch.nn.parallel.DistributedDataParallel, torch.nn.DataParallel),
                ):
                    attr_value = attr_value.module
                torch.save(
                    attr_value.state_dict(), os.path.join(dirname, "{}.pt".format(attr))
                )
        ancestors.pop()

    def load(self, dirname: str) -> None:
        """Load internal states."""
        self.__load(dirname, [])

    def __load(self, dirname: str, ancestors: List[Any]) -> None:
        map_location = torch.device("cpu") if not torch.cuda.is_available() else None
        ancestors.append(self)
        for attr in self.saved_attributes:
            assert hasattr(self, attr)
            attr_value = getattr(self, attr)
            if attr_value is None:
                continue
            if isinstance(attr_value, AttributeSavingMixin):
                assert not any(
                    attr_value is ancestor for ancestor in ancestors
                ), "Avoid an infinite loop"
                attr_value.load(os.path.join(dirname, attr))
            else:
                if isinstance(
                    attr_value,
                    (torch.nn.parallel.DistributedDataParallel, torch.nn.DataParallel),
                ):
                    attr_value = attr_value.module
                attr_value.load_state_dict(
                    torch.load(
                        os.path.join(dirname, "{}.pt".format(attr)), map_location
                    )
                )
        ancestors.pop()


def _mean_or_nan(xs: Sequence[float]) -> float:
    """Return its mean a non-empty sequence, numpy.nan for a empty one."""
    return typing.cast(float, np.mean(xs)) if xs else np.nan


def compute_value_loss(
    y: torch.Tensor,
    t: torch.Tensor,
    clip_delta: bool = True,
    batch_accumulator: str = "mean",
) -> torch.Tensor:
    """Compute a loss for value prediction problem.

    Args:
        y (torch.Tensor): Predicted values.
        t (torch.Tensor): Target values.
        clip_delta (bool): Use the Huber loss function with delta=1 if set True.
        batch_accumulator (str): 'mean' or 'sum'. 'mean' will use the mean of
            the loss values in a batch. 'sum' will use the sum.
    Returns:
        (torch.Tensor) scalar loss
    """
    assert batch_accumulator in ("mean", "sum")
    y = y.reshape(-1, 1)
    t = t.reshape(-1, 1)
    if clip_delta:
        return F.smooth_l1_loss(y, t, reduction=batch_accumulator)
    else:
        return F.mse_loss(y, t, reduction=batch_accumulator) / 2


def compute_weighted_value_loss(
    y: torch.Tensor,
    t: torch.Tensor,
    weights: torch.Tensor,
    clip_delta: bool = True,
    batch_accumulator: str = "mean",
) -> torch.Tensor:
    """Compute a loss for value prediction problem.

    Args:
        y (torch.Tensor): Predicted values.
        t (torch.Tensor): Target values.
        weights (torch.Tensor): Weights for y, t.
        clip_delta (bool): Use the Huber loss function with delta=1 if set True.
        batch_accumulator (str): 'mean' will divide loss by batchsize
    Returns:
        (torch.Tensor) scalar loss
    """
    assert batch_accumulator in ("mean", "sum")
    y = y.reshape(-1, 1)
    t = t.reshape(-1, 1)
    if clip_delta:
        losses = F.smooth_l1_loss(y, t, reduction="none")
    else:
        losses = F.mse_loss(y, t, reduction="none") / 2
    losses = losses.reshape(
        -1,
    )
    weights = weights.to(losses.device)
    loss_sum = torch.sum(losses * weights)
    if batch_accumulator == "mean":
        loss = loss_sum / y.shape[0]
    elif batch_accumulator == "sum":
        loss = loss_sum
    return loss


def make_target_model_as_copy(model: torch.nn.Module) -> torch.nn.Module:
    target_model = copy.deepcopy(model)

    def flatten_parameters(mod):
        if isinstance(mod, torch.nn.RNNBase):
            mod.flatten_parameters()

    # RNNBase.flatten_parameters must be called again after deep-copy.
    # See: https://discuss.pytorch.org/t/why-do-we-need-flatten-parameters-when-using-rnn-with-dataparallel/46506  # NOQA
    target_model.apply(flatten_parameters)
    # set target n/w to evaluate only.
    target_model.eval()
    return target_model


class DQN(agent.AttributeSavingMixin, agent.Agent):
    """Deep Q-Network algorithm.

    Args:
        q_function (StateQFunction): Q-function
        optimizer (Optimizer): Optimizer that is already setup
        replay_buffer (ReplayBuffer): Replay buffer
        gamma (float): Discount factor
        gpu (int): GPU device id if not None nor negative.
        replay_start_size (int): if the replay buffer's size is less than
            replay_start_size, skip update
        minibatch_size (int): Minibatch size
        update_interval (int): Model update interval in step
        target_update_interval (int): Target model update interval in step
        clip_delta (bool): Clip delta if set True
        phi (callable): Feature extractor applied to observations
        target_update_method (str): 'hard' or 'soft'.
        soft_update_tau (float): Tau of soft target update.
        n_times_update (int): Number of repetition of update
        batch_accumulator (str): 'mean' or 'sum'
        episodic_update_len (int or None): Subsequences of this length are used
            for update if set int and episodic_update=True
        logger (Logger): Logger used
        batch_states (callable): method which makes a batch of observations.
            default is `pfrl.utils.batch_states.batch_states`
        recurrent (bool): If set to True, `model` is assumed to implement
            `pfrl.nn.Recurrent` and is updated in a recurrent
            manner.
        max_grad_norm (float or None): Maximum L2 norm of the gradient used for
            gradient clipping. If set to None, the gradient is not clipped.
    """

    saved_attributes = ("model", "target_model", "optimizer")

    def __init__(
        self,
        n_actions: int,
        q_function: torch.nn.Module,
        optimizer: torch.optim.Optimizer,  # type: ignore  # somehow mypy complains
        replay_buffer: rb.AbstractReplayBuffer,
        gamma: float,
        gpu: Optional[int] = None,
        replay_start_size: int = 50000,
        minibatch_size: int = 32,
        update_interval: int = 1,
        target_update_interval: int = 10000,
        clip_delta: bool = True,
        phi: Callable[[Any], Any] = lambda x: x,
        target_update_method: str = "hard",
        soft_update_tau: float = 1e-2,
        n_times_update: int = 1,
        batch_accumulator: str = "mean",
        episodic_update_len: Optional[int] = None,
        logger: Logger = getLogger(__name__),
        batch_states: Callable[
            [Sequence[Any], torch.device, Callable[[Any], Any]], Any
        ] = batch_states.batch_states,
        recurrent: bool = False,
        max_grad_norm: Optional[float] = None,
        epsilon: float = 0.1
    ):
        self.n_actions = n_actions
        self.model = q_function

        if gpu is not None and gpu >= 0:
            assert torch.cuda.is_available()
            self.device = torch.device("cuda:{}".format(gpu))
            self.model.to(self.device)
        else:
            self.device = torch.device("cpu")

        self.replay_buffer = replay_buffer
        self.optimizer = optimizer
        self.gamma = gamma
        self.gpu = gpu
        self.target_update_interval = target_update_interval
        self.clip_delta = clip_delta
        self.phi = phi
        self.target_update_method = target_update_method
        self.soft_update_tau = soft_update_tau
        self.batch_accumulator = batch_accumulator
        assert batch_accumulator in ("mean", "sum")
        self.logger = logger
        self.batch_states = batch_states
        self.recurrent = recurrent

        self.minibatch_size = minibatch_size
        self.episodic_update_len = episodic_update_len
        self.replay_start_size = replay_start_size
        self.update_interval = update_interval
        self.max_grad_norm = max_grad_norm

        assert (
            target_update_interval % update_interval == 0
        ), "target_update_interval should be a multiple of update_interval"

        self.t = 0
        self.optim_t = 0  # Compensate pytorch optim not having `t`
        self._cumulative_steps = 0
        self.target_model = make_target_model_as_copy(self.model)

        # Statistics
        self.q_record: collections.deque = collections.deque(maxlen=1000)
        self.loss_record: collections.deque = collections.deque(maxlen=100)

        # Recurrent states of the model
        self.train_recurrent_states: Any = None
        self.train_prev_recurrent_states: Any = None
        self.test_recurrent_states: Any = None

        # Error checking
        if (
            self.replay_buffer.capacity is not None
            and self.replay_buffer.capacity < replay_start_size
        ):
            raise ValueError("Replay start size cannot exceed replay buffer capacity.")
        self.epsilon = epsilon

    def observe(self, obs: Any, reward: float, done: bool, reset: bool) -> None:
        self.batch_observe([obs], [reward], [done], [reset])

    @property
    def cumulative_steps(self) -> int:
        # cumulative_steps counts the overall steps during the training.
        return self._cumulative_steps

    def sync_target_network(self) -> None:
        """Synchronize target network with current network."""
        copy_param.synchronize_parameters(
            src=self.model,
            dst=self.target_model,
            method=self.target_update_method,
            tau=self.soft_update_tau,
        )

    def _compute_target_values(self, exp_batch: Dict[str, Any]) -> torch.Tensor:
        batch_next_state = exp_batch["next_state"]

        target_next_qout = self.target_model(batch_next_state)
        next_q_max, _ = target_next_qout.max(1)

        batch_rewards = exp_batch["reward"]
        batch_terminal = exp_batch["is_state_terminal"]
        discount = exp_batch["discount"]
        return batch_rewards + discount * (1.0 - batch_terminal) * next_q_max

    def _compute_y_and_t(
        self, exp_batch: Dict[str, Any]
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        batch_size = exp_batch["reward"].shape[0]

        # Compute Q-values for current states
        batch_state = exp_batch["state"]

        qout = self.model(batch_state)

        batch_actions = exp_batch["action"]
        index = batch_actions.long().unsqueeze(1)
        raw_q_values = qout.gather(dim=1, index=index).flatten()
        batch_q = torch.reshape(raw_q_values, (batch_size, 1))
        with torch.no_grad():
            batch_q_target = torch.reshape(
                self._compute_target_values(exp_batch), (batch_size, 1)
            )

        return batch_q, batch_q_target

    def _compute_loss(
        self, exp_batch: Dict[str, Any], errors_out: Optional[list] = None
    ) -> torch.Tensor:
        """Compute the Q-learning loss for a batch of experiences


        Args:
          exp_batch (dict): A dict of batched arrays of transitions
        Returns:
          Computed loss from the minibatch of experiences
        """
        y, t = self._compute_y_and_t(exp_batch)

        self.q_record.extend(y.detach().cpu().numpy().ravel())

        if errors_out is not None:
            del errors_out[:]
            delta = torch.abs(y - t)
            if delta.ndim == 2:
                delta = torch.sum(delta, dim=1)
            delta = delta.detach().cpu().numpy()
            for e in delta:
                errors_out.append(e)

        if "weights" in exp_batch:
            return compute_weighted_value_loss(
                y,
                t,
                exp_batch["weights"],
                clip_delta=self.clip_delta,
                batch_accumulator=self.batch_accumulator,
            )
        else:
            return compute_value_loss(
                y,
                t,
                clip_delta=self.clip_delta,
                batch_accumulator=self.batch_accumulator,
            )

    def act(self, obs: Any) -> Any:
        if self.training and np.random.rand() < self.epsilon:
            return np.random.randint(self.n_actions), float('nan')

        with torch.no_grad(), contexts.evaluating(self.model):
            batch_xs = self.batch_states(np.expand_dims(obs, 0), self.device, self.phi)
            q_values = self.model(batch_xs)
            max_q, max_index = q_values.detach().max(1)
            return max_index.int().item(), max_q.float().item()

    def remember(self, obs: Any, aciton: Any, next_obs: Any, reward: float, done: bool) -> None:
        self.t += 1
        self._cumulative_steps += 1

        # Add a transition to the replay buffer
        transition = {
            "state": obs,
            "action": aciton,
            "reward": reward,
            "next_state": next_obs,
            "next_action": None,
            "is_state_terminal": done,
        }

        self.replay_buffer.append(env_id=0, **transition)

    def train(self):
        if len(self.replay_buffer) < self.replay_start_size:
            return

        transitions = self.replay_buffer.sample(self.minibatch_size)
        exp_batch = rb.batch_experiences(
            transitions,
            device=self.device,
            phi=self.phi,
            gamma=self.gamma,
            batch_states=self.batch_states,
        )

        loss = self._compute_loss(exp_batch)

        self.loss_record.append(float(loss.detach().cpu().numpy()))

        self.optimizer.zero_grad()
        loss.backward()
        self.optimizer.step()
        self.optim_t += 1

        # Update the target network
        if self.optim_t > 0 and self.optim_t % self.target_update_interval == 0:
            print(f"sync_target_network {self.optim_t}")
            self.sync_target_network()

    def save_snapshot(self, dirname: str) -> None:
        self.save(dirname)
        torch.save(self.t, os.path.join(dirname, "t.pt"))
        torch.save(self.optim_t, os.path.join(dirname, "optim_t.pt"))
        torch.save(
            self._cumulative_steps, os.path.join(dirname, "_cumulative_steps.pt")
        )
        self.replay_buffer.save(os.path.join(dirname, "replay_buffer.pkl"))

    def load_snapshot(self, dirname: str) -> None:
        self.load(dirname)
        self.t = torch.load(os.path.join(dirname, "t.pt"))
        self.optim_t = torch.load(os.path.join(dirname, "optim_t.pt"))
        self._cumulative_steps = torch.load(
            os.path.join(dirname, "_cumulative_steps.pt")
        )
        self.replay_buffer.load(os.path.join(dirname, "replay_buffer.pkl"))

    def get_statistics(self):
        return [
            ("average_q", _mean_or_nan(self.q_record)),
            ("average_loss", _mean_or_nan(self.loss_record)),
            ("cumulative_steps", self.cumulative_steps),
            ("n_updates", self.optim_t),
            ("rlen", len(self.replay_buffer)),
        ]
