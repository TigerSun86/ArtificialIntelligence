import argparse
import json
import os
import numpy as np
import torch.nn as nn
import random_seed
import prepare_output_dir
import torch
import atari_wrappers
import randomize_action
import render
import atari_cnn
import chainer_default
from abc import ABCMeta, abstractmethod, abstractproperty
from functools import update_wrapper
import dqn
import train_agent
import action_value
import explorer
import replay_buffer


def select_action_epsilon_greedily(epsilon, random_action_func, greedy_action_func):
    if np.random.rand() < epsilon:
        return random_action_func(), False
    else:
        return greedy_action_func(), True


class LinearDecayEpsilonGreedy(explorer.Explorer):
    """Epsilon-greedy with linearly decayed epsilon

    Args:
      start_epsilon: max value of epsilon
      end_epsilon: min value of epsilon
      decay_steps: how many steps it takes for epsilon to decay
      random_action_func: function with no argument that returns action
      logger: logger used
    """

    def __init__(
        self,
        start_epsilon,
        end_epsilon,
        decay_steps,
        random_action_func,
        # logger=getLogger(__name__),
    ):
        assert start_epsilon >= 0 and start_epsilon <= 1
        assert end_epsilon >= 0 and end_epsilon <= 1
        assert decay_steps >= 0
        self.start_epsilon = start_epsilon
        self.end_epsilon = end_epsilon
        self.decay_steps = decay_steps
        self.random_action_func = random_action_func
        # self.logger = logger
        self.epsilon = start_epsilon

    def compute_epsilon(self, t):
        if t > self.decay_steps:
            return self.end_epsilon
        else:
            epsilon_diff = self.end_epsilon - self.start_epsilon
            return self.start_epsilon + epsilon_diff * (t / self.decay_steps)

    def select_action(self, t, greedy_action_func, action_value=None):
        self.epsilon = self.compute_epsilon(t)
        a, greedy = select_action_epsilon_greedily(
            self.epsilon, self.random_action_func, greedy_action_func
        )
        greedy_str = "greedy" if greedy else "non-greedy"
        # self.logger.debug("t:%s a:%s %s", t, a, greedy_str)
        return a

    def __repr__(self):
        return "LinearDecayEpsilonGreedy(epsilon={})".format(self.epsilon)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--env",
        type=str,
        default="BreakoutNoFrameskip-v4",
        help="OpenAI Atari domain to perform algorithm on.",
    )
    parser.add_argument(
        "--outdir",
        type=str,
        default="results",
        help=(
            "Directory path to save output files."
            " If it does not exist, it will be created."
        ),
    )
    parser.add_argument("--seed", type=int, default=0, help="Random seed [0, 2 ** 31)")
    parser.add_argument(
        "--gpu", type=int, default=0, help="GPU to use, set to -1 if no GPU."
    )
    parser.add_argument("--demo", action="store_true", default=False)
    parser.add_argument("--load-pretrained", action="store_true", default=False)
    parser.add_argument(
        "--pretrained-type", type=str, default="best", choices=["best", "final"]
    )
    parser.add_argument("--load", type=str, default=None)
    parser.add_argument(
        "--log-level",
        type=int,
        default=20,
        help="Logging level. 10:DEBUG, 20:INFO etc.",
    )
    parser.add_argument(
        "--render",
        action="store_true",
        default=False,
        help="Render env states in a GUI window.",
    )
    parser.add_argument(
        "--monitor",
        action="store_true",
        default=False,
        help=(
            "Monitor env. Videos and additional information are saved as output files."
        ),
    )
    parser.add_argument(
        "--steps",
        type=int,
        default=5 * 10**7,
        help="Total number of timesteps to train the agent.",
    )
    parser.add_argument(
        "--replay-start-size",
        type=int,
        default=5 * 10**4,
        help="Minimum replay buffer size before " + "performing gradient updates.",
    )
    parser.add_argument("--eval-n-steps", type=int, default=125000)
    parser.add_argument("--eval-interval", type=int, default=250000)
    parser.add_argument("--n-best-episodes", type=int, default=30)
    args = parser.parse_args()

    import logging

    logging.basicConfig(level=args.log_level)

    # Set a random seed used in PFRL.
    random_seed.set_random_seed(args.seed)

    # Set different random seeds for train and test envs.
    train_seed = args.seed
    test_seed = 2**31 - 1 - args.seed

    args.outdir = prepare_output_dir.prepare_output_dir(args, args.outdir)
    print("Output files are saved in {}".format(args.outdir))

    def make_env(test):
        # Use different random seeds for train and test envs
        env_seed = test_seed if test else train_seed
        env = atari_wrappers.wrap_deepmind(
            atari_wrappers.make_atari(args.env, max_frames=None),
            episode_life=not test,
            clip_rewards=not test,
        )
        env.seed(int(env_seed))
        if test:
            # Randomize actions like epsilon-greedy in evaluation as well
            env = randomize_action.RandomizeAction(env, 0.05)
        if args.render:
            env = render.Render(env)
        return env

    env = make_env(test=False)
    eval_env = make_env(test=True)

    class DiscreteActionValueHead(nn.Module):
        def forward(self, q_values):
            return action_value.DiscreteActionValue(q_values)

    n_actions = env.action_space.n
    q_func = nn.Sequential(
        atari_cnn.SmallAtariCNN(),
        chainer_default.init_chainer_default(nn.Linear(256, n_actions)),
        DiscreteActionValueHead(),
    )

    # Use the same hyperparameters as the Nature paper

    # opt = pfrl.optimizers.RMSpropEpsInsideSqrt(
    #     q_func.parameters(),
    #     lr=2.5e-4,
    #     alpha=0.95,
    #     momentum=0.0,
    #     eps=1e-2,
    #     centered=True,
    # )

    opt = torch.optim.Adam(q_func.parameters(), lr=2.5e-4)

    rbuf = replay_buffer.ReplayBuffer(10**6)

    explorer = LinearDecayEpsilonGreedy(
        start_epsilon=1.0,
        end_epsilon=0.1,
        decay_steps=10**6,
        random_action_func=lambda: np.random.randint(n_actions),
    )

    def phi(x):
        # Feature extractor
        return np.asarray(x, dtype=np.float32) / 255

    Agent = dqn.DQN
    agent = Agent(
        q_func,
        opt,
        rbuf,
        gpu=args.gpu,
        gamma=0.99,
        explorer=explorer,
        replay_start_size=args.replay_start_size,
        target_update_interval=10**4,
        clip_delta=True,
        update_interval=1,
        batch_accumulator="sum",
        phi=phi,
    )

    train_agent.train_agent_with_evaluation(
        agent=agent,
        env=env,
        steps=args.steps,
        eval_n_steps=args.eval_n_steps,
        eval_n_episodes=None,
        eval_interval=args.eval_interval,
        outdir=args.outdir,
        save_best_so_far_agent=True,
        eval_env=eval_env,
    )


if __name__ == "__main__":
    main()
