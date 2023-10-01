import datetime
import logging
import os

import numpy as np

from torch.utils.tensorboard import SummaryWriter


def save_agent_replay_buffer(agent, t, outdir, suffix="", logger=None):
    logger = logger or logging.getLogger(__name__)
    filename = os.path.join(outdir, "{}{}.replay.pkl".format(t, suffix))
    agent.replay_buffer.save(filename)
    logger.info("Saved the current replay buffer to %s", filename)


def save_agent(agent, t, outdir, logger, suffix=""):
    dirname = os.path.join(outdir, "{}{}".format(t, suffix))
    agent.save(dirname)
    logger.info("Saved the agent to %s", dirname)


def train_agent(
    agent,
    env,
    steps,
    outdir,
    checkpoint_freq=None,
    max_episode_len=None,
    step_offset=0,
    evaluator=None,
    successful_score=None,
    step_hooks=(),
    eval_during_episode=False,
    logger=None,
):
    logger = logger or logging.getLogger(__name__)

    episode_r = 0
    episode_idx = 0

    # o_0, r_0
    obs = env.reset()

    t = step_offset
    if hasattr(agent, "t"):
        agent.t = step_offset

    eval_stats_history = []  # List of evaluation episode stats dict
    episode_len = 0

    timestr = datetime.datetime.now().strftime("%Y-%m-%d_%I-%M-%S")
    writer = SummaryWriter(f'./logs/pong_{timestr}')
    # print(type(agent.model[0]))
    # in_obs = np.zeros_like(obs)
    # writer.add_graph(agent.model[0], in_obs)
    has_trained = False
    ex_t = 0
    try:
        while t < steps:
            while True:
                # a_t
                action = agent.act(obs)
                # o_{t+1}, r_{t+1}
                obs, r, done, info = env.step(action)
                t += 1
                episode_r += r
                episode_len += 1
                reset = episode_len == max_episode_len or info.get("needs_reset", False)
                # agent.observe(obs, r, done, reset)
                agent.remember(obs, r, done, reset)

                for hook in step_hooks:
                    hook(env, agent, t)

                episode_end = done or reset or t == steps
                if episode_end:
                    break

            logger.info(
                "step:%s episode_len: %s episode:%s R:%s",
                t,
                episode_len,
                episode_idx,
                episode_r,
            )

            loop_count = episode_len
            if not has_trained and len(agent.replay_buffer) >= agent.replay_start_size:
                has_trained = True
                loop_count = 3 * 10 ** 5

            for i in range(loop_count):
                agent.learn()
                ex_t += 1
                if loop_count > episode_len and i % (loop_count / 1000) == 0:
                    logger.info("trained:%s/%s", i, loop_count)
                    stats = agent.get_statistics()
                    logger.info("statistics:%s", stats)
                    stats_dict = dict(stats)
                    if writer:
                        writer.add_scalar('episode_reward', episode_r, t + ex_t)
                        writer.add_scalar('average_q', stats_dict['average_q'], t + ex_t)
                        writer.add_scalar('average_loss', stats_dict['average_loss'], t + ex_t)

            stats = agent.get_statistics()
            logger.info("statistics:%s", stats)
            stats_dict = dict(stats)
            if writer:
                writer.add_scalar('episode_reward', episode_r, t + ex_t)
                writer.add_scalar('average_q', stats_dict['average_q'], t + ex_t)
                writer.add_scalar('average_loss', stats_dict['average_loss'], t+ex_t)
            # if writer:
            #     writer.add_scalar('episode_reward', episode_r, t)
            #     writer.add_scalar('average_q', stats_dict['average_q'], t)
            #     writer.add_scalar('average_actor_loss', stats_dict['average_actor_loss'], t)
            #     writer.add_scalar('average_critic_loss', stats_dict['average_critic_loss'], t)
            episode_idx += 1

            if t == steps:
                break
            # Start a new episode
            episode_r = 0
            episode_len = 0
            obs = env.reset()
            if checkpoint_freq and t % checkpoint_freq == 0:
                save_agent(agent, t, outdir, logger, suffix="_checkpoint")
    except (Exception, KeyboardInterrupt):
        # Save the current model before being killed
        save_agent(agent, t, outdir, logger, suffix="_except")
        raise

    # Save the final model
    save_agent(agent, t, outdir, logger, suffix="_finish")

    return eval_stats_history


def train_agent_with_evaluation(
    agent,
    env,
    steps,
    eval_n_steps,
    eval_n_episodes,
    eval_interval,
    outdir,
    checkpoint_freq=None,
    train_max_episode_len=None,
    step_offset=0,
    eval_max_episode_len=None,
    eval_env=None,
    successful_score=None,
    step_hooks=(),
    evaluation_hooks=(),
    save_best_so_far_agent=True,
    use_tensorboard=False,
    eval_during_episode=False,
    logger=None,
):
    """Train an agent while periodically evaluating it.

    Args:
        agent: A pfrl.agent.Agent
        env: Environment train the agent against.
        steps (int): Total number of timesteps for training.
        eval_n_steps (int): Number of timesteps at each evaluation phase.
        eval_n_episodes (int): Number of episodes at each evaluation phase.
        eval_interval (int): Interval of evaluation.
        outdir (str): Path to the directory to output data.
        checkpoint_freq (int): frequency at which agents are stored.
        train_max_episode_len (int): Maximum episode length during training.
        step_offset (int): Time step from which training starts.
        eval_max_episode_len (int or None): Maximum episode length of
            evaluation runs. If None, train_max_episode_len is used instead.
        eval_env: Environment used for evaluation.
        successful_score (float): Finish training if the mean score is greater
            than or equal to this value if not None
        step_hooks (Sequence): Sequence of callable objects that accepts
            (env, agent, step) as arguments. They are called every step.
            See pfrl.experiments.hooks.
        evaluation_hooks (Sequence): Sequence of
            pfrl.experiments.evaluation_hooks.EvaluationHook objects. They are
            called after each evaluation.
        save_best_so_far_agent (bool): If set to True, after each evaluation
            phase, if the score (= mean return of evaluation episodes) exceeds
            the best-so-far score, the current agent is saved.
        use_tensorboard (bool): Additionally log eval stats to tensorboard
        eval_during_episode (bool): Allow running evaluation during training episodes.
            This should be enabled only when `env` and `eval_env` are independent.
        logger (logging.Logger): Logger used in this function.
    Returns:
        agent: Trained agent.
        eval_stats_history: List of evaluation episode stats dict.
    """

    logger = logger or logging.getLogger(__name__)

    for hook in evaluation_hooks:
        if not hook.support_train_agent:
            raise ValueError(
                "{} does not support train_agent_with_evaluation().".format(hook)
            )

    os.makedirs(outdir, exist_ok=True)

    if eval_env is None:
        assert not eval_during_episode, (
            "To run evaluation during training episodes, you need to specify `eval_env`"
            " that is independent from `env`."
        )
        eval_env = env

    if eval_max_episode_len is None:
        eval_max_episode_len = train_max_episode_len

    eval_stats_history = train_agent(
        agent,
        env,
        steps,
        outdir,
        checkpoint_freq=checkpoint_freq,
        max_episode_len=train_max_episode_len,
        step_offset=step_offset,
        evaluator=None,
        successful_score=successful_score,
        step_hooks=step_hooks,
        eval_during_episode=eval_during_episode,
        logger=logger,
    )

    return agent, eval_stats_history
