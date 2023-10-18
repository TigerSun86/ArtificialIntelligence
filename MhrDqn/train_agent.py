from collections import deque
import datetime
import logging
import os
import time
import torch
import numpy as np
import common_definitions
import dqn
import mh_env

from torch.utils.tensorboard.writer import SummaryWriter


def save_agent_replay_buffer(agent, t, outdir, suffix="", logger=None):
    logger = logger or logging.getLogger(__name__)
    filename = os.path.join(outdir, "{}{}.replay.pkl".format(t, suffix))
    agent.replay_buffer.save(filename)
    logger.info("Saved the current replay buffer to %s", filename)


def save_agent(agent, t, outdir, logger, suffix=""):
    dirname = os.path.join(outdir, "{}{}".format(t, suffix))
    agent.save(dirname)
    logger.info("Saved the agent to %s", dirname)


def log_model_weights(writer, agent, log_step):
    # writer.add_scalar('Epsilon', agent.epsilon, log_step)
    for name, p in agent.model.named_parameters():
        layer, attr = name.split(".", 1)
        writer.add_histogram(f'model.{layer}/{attr}', p,  log_step, bins='auto')
        if p.grad != None:
            writer.add_histogram(f'model.{layer}/{attr}.grad', p.grad,  log_step, bins='auto')

    for name, p in agent.target_model.named_parameters():
        layer, attr = name.split(".", 1)
        writer.add_histogram(f'target.{layer}/{attr}', p, log_step, bins='auto')


def train_agent(
    agent: dqn.DQN,
    env: mh_env.MhEnv,
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

    episode_reward = 0
    episode_idx = 0

    t = step_offset

    eval_stats_history = []  # List of evaluation episode stats dict
    episode_len = 0
    quest_step = 0
    log_step = 0

    env.reset_debug_ui()
    env.render()

    env.start_quest(wait_before_start=True)
    timestr = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M")
    writer = SummaryWriter(f'./logs/mhr_{timestr}')
    try:
        log_model_weights(writer, agent, log_step)
        while t < steps:
            total_time = 0.
            last_time = time.time()
            episode_steps = 0
            q_sum = 0.
            obs = env.reset()

            while True:
                action, q_value = agent.act(obs)
                next_obs, done, time_slept = env.step(action)
                env.render()

                t += 1
                episode_steps += 1
                episode_len += 1
                quest_step += 1
                reset = episode_len == max_episode_len
                obs = next_obs

                q_sum += q_value if not np.isnan(q_value) else 0

                print('step {}, {} took {:.3f} s, slept {:.3f} s, q: {:.2E}, a: {} ({})'.format(episode_steps,
                      t, time.time()-last_time, time_slept, q_value, action, env.dqn_action_to_str(action)))
                total_time += time.time()-last_time
                last_time = time.time()

                episode_end = done or reset or t == steps
                if episode_end:
                    break

            env.release_all_buttons()

            print('episode {} took {}/{} steps, {:.3f} seconds, avg took {:.3f} seconds, q_avg {:.2E}'.format(
                episode_idx, episode_steps, t, total_time, total_time / episode_steps, q_sum / episode_steps))

            if not done:
                env.pause_game()

            for obs, action, next_obs, reward, done2 in env.get_examples():
                agent.remember(obs, action, next_obs, reward, done2)
                episode_reward += reward

            last_time = time.time()
            log_step = t - episode_len
            loop_count = episode_len
            for i in range(loop_count):
                agent.train()
                log_step += 1
                if log_step % 10000 == 0:
                    log_model_weights(writer, agent, log_step)

            stats = agent.get_statistics()
            logger.info("training took %f seconds, statistics:%s", time.time()-last_time, stats)
            stats_dict = dict(stats)
            if writer:
                writer.add_scalar('average_step_reward', episode_reward / episode_len, log_step)
                writer.add_scalar('episode_reward', episode_reward, log_step)
                writer.add_scalar('average_q', stats_dict['average_q'], log_step)
                writer.add_scalar('average_loss', stats_dict['average_loss'], log_step)
                writer.add_scalar('episode_len', episode_len, log_step)
            episode_idx += 1

            if t == steps:
                break

            if not done:
                env.resume_game()

            if done:
                writer.add_scalar('quest_step', quest_step, log_step)
                quest_step = 0
                env.exit_quest()
                env.start_quest()

            # Start a new episode
            episode_reward = 0
            episode_len = 0
            if checkpoint_freq and t % checkpoint_freq == 0:
                save_agent(agent, t, outdir, logger, suffix="_checkpoint")
    except (Exception, KeyboardInterrupt):
        # Save the current model before being killed
        save_agent(agent, t, outdir, logger, suffix="_except")
        raise

    # Save the final model
    save_agent(agent, t, outdir, logger, suffix="_finish")

    return eval_stats_history


def demo(
    agent: dqn.DQN,
    env: mh_env.MhEnv,
):
    t = 0

    env.reset_debug_ui()
    env.render()

    env.start_quest(wait_before_start=True)
    last_time = time.time()

    obs = env.reset()

    while True:
        action, q_value = agent.act(obs)
        next_obs, done, time_slept = env.step(action)
        env.render()

        t += 1
        obs = next_obs

        print('step {} took {:.3f} s, slept {:.3f} s, q: {:.2E}, a: {} ({})'.format(
            t, time.time()-last_time, time_slept, q_value, action, env.dqn_action_to_str(action)))
        last_time = time.time()

        if done:
            break

    env.release_all_buttons()
    env.exit_quest()


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
