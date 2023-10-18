from collections import deque
import os
import time

import cv2
import numpy as np
from gym import spaces
from mod_reward_judge import ModRewardJudge

import directkeys as keys
import grabscreenv2
from display_text import DisplayText
import common_definitions
import mhr_game_operator

KEY_MOVE_FORWARD = keys.KEY_W
KEY_MOVE_BACKWARDS = keys.KEY_S
KEY_MOVE_LEFT = keys.KEY_A
KEY_MOVE_RIGHT = keys.KEY_D
KEY_DASH = keys.KEY_LSHIFT
KEY_DODGE = keys.KEY_SPACE
KEY_NORMAL_ATTACK = keys.KEY_Y
KEY_SPECIAL_ATTACK = keys.KEY_U
KEY_GUARD = keys.KEY_I
KEY_WIREBUG_RETICLE = keys.KEY_C
KEY_USE_ITEM = keys.KEY_E

KEY_ITEM_BAR = keys.KEY_CTRL
KEY_UP = keys.KEY_UP_8
KEY_LEFT = keys.KEY_LEFT_4
KEY_RIGHT = keys.KEY_RIGHT_6
KEY_DOWN = keys.KEY_DOWN_2
KEY_EXAMINE = keys.KEY_F
KEY_OPEN_START_MENU = keys.KEY_ESC
KEY_OPEN_DETAILED_MAP = keys.KEY_M
KEY_LOCK_ON_TARGET = keys.KEY_Q

KEY_TO_STR = {KEY_MOVE_FORWARD: "W",
              KEY_MOVE_BACKWARDS: "S",
              KEY_MOVE_LEFT: "A",
              KEY_MOVE_RIGHT: "D",
              KEY_DASH: "SHIFT",
              KEY_DODGE: "SPACE",
              KEY_NORMAL_ATTACK: "Y",
              KEY_SPECIAL_ATTACK: "U",
              KEY_GUARD: "I",
              KEY_WIREBUG_RETICLE: "C",
              KEY_USE_ITEM: "E"}


class MhEnv:
    def __init__(self, outdir):
        self.elapsed_steps = 0
        self.episode_examples = deque(maxlen=int(common_definitions.EPISODE_STEP_COUNT + 1))

        self.operator = mhr_game_operator.MhrGameOperator()

        self.save_img = False

        # self.buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT, KEY_MOVE_RIGHT, KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        # self.enabled_buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT, KEY_MOVE_RIGHT, KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        self.buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT,
                        KEY_MOVE_RIGHT, KEY_DODGE, KEY_NORMAL_ATTACK)
        self.enabled_buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT,
                                KEY_MOVE_RIGHT, KEY_DODGE, KEY_NORMAL_ATTACK)
        # self.buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT, KEY_MOVE_RIGHT, KEY_DASH,
        #                 KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK, KEY_GUARD, KEY_WIREBUG_RETICLE, KEY_USE_ITEM)
        # self.enabled_buttons = (KEY_MOVE_FORWARD, KEY_MOVE_LEFT, KEY_MOVE_RIGHT,
        #                         KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        self.button_states = np.full(len(self.buttons), False, dtype=bool)

        self.dqn_actions = [{},
                            {KEY_MOVE_FORWARD},
                            {KEY_MOVE_BACKWARDS},
                            {KEY_MOVE_LEFT},
                            {KEY_MOVE_RIGHT},
                            {KEY_MOVE_FORWARD, KEY_NORMAL_ATTACK},
                            {KEY_MOVE_BACKWARDS, KEY_NORMAL_ATTACK},
                            {KEY_MOVE_LEFT, KEY_NORMAL_ATTACK},
                            {KEY_MOVE_RIGHT, KEY_NORMAL_ATTACK},
                            {KEY_MOVE_FORWARD, KEY_DODGE},
                            {KEY_MOVE_BACKWARDS, KEY_DODGE},
                            {KEY_MOVE_LEFT, KEY_DODGE},
                            {KEY_MOVE_RIGHT, KEY_DODGE},
                            ]

        self.time_between_steps = 0.1
        self.last_step_time = time.time()

        # self.action_num = len(self.buttons)
        self.action_num = len(self.dqn_actions)
        self.action_min = 0.
        self.action_max = 1.

        y1 = 0
        y2 = common_definitions.GAME_SCREEN_RESOLUTION[1]
        x1 = int((common_definitions.GAME_SCREEN_RESOLUTION[0] - common_definitions.GAME_SCREEN_RESOLUTION[1]) / 2)
        x2 = x1 + common_definitions.GAME_SCREEN_RESOLUTION[1]
        self.window_size = [y1, y2, x1, x2]  # y1, y2, x1, x2
        # self.window_size = (70, 400, 100, 540)  # y1, y2, x1, x2
        self.img_width = 180
        self.img_height = 180
        self.img_min = 0.
        self.img_max = 1.

        # action_low = np.full(
        #     self.action_num, self.action_min, dtype=np.float32)
        # action_high = np.full(
        #     self.action_num, self.action_max, dtype=np.float32)

        # self.action_space = spaces.Box(
        #     low=action_low,
        #     high=action_high, shape=(self.action_num,),
        #     dtype=np.float32
        # )

        # observation_low = np.full(
        #     (self.img_height, self.img_width), self.img_min, dtype=np.float32)
        # observation_high = np.full(
        #     (self.img_height, self.img_width), self.img_max, dtype=np.float32)

        # self.observation_space = spaces.Box(
        #     low=-observation_low,
        #     high=observation_high, shape=(self.img_height, self.img_width,),
        #     dtype=np.float32
        # )

        self.last_obs = None
        self.is_rendering = False
        self.synced_timestamp = None
        self.k = 4
        self.frames = deque([], maxlen=self.k)
        self.is_save_screenshot = True
        self.outdir = outdir
        self.quest_idx = 0
        self.episode_idx = 0

        self.judge = ModRewardJudge(common_definitions.GAME_LOG_PATH)

        self.display_text = DisplayText()

    def get_action_number(self):
        return self.action_num

    def step(self, action):
        assert self.elapsed_steps is not None, "Cannot call env.step() before calling reset()"
        assert self.synced_timestamp is not None, "Cannot call env.step() before sync time with game"

        # self.perform_action(action)
        self.dqn_perform_action(action)

        self.elapsed_steps += 1

        cur_time = time.time()
        difference = cur_time - self.last_step_time
        time_to_sleep = self.time_between_steps - difference
        if (time_to_sleep > 0):
            time.sleep(time_to_sleep)

        self.judge.load_file_to_buffer()
        done = self.judge.is_quest_end
        self.last_step_time = time.time()
        obs = self.format_img_for_training(self.screenshot())
        self.frames.append(obs)
        assert len(self.frames) == self.k
        self.last_obs = np.array(self.frames)
        self.episode_examples.append({
            "end_time": self.last_step_time - self.synced_timestamp,
            "action": action,
            "next_obs": self.last_obs,
            "done": done,
        })

        return self.last_obs, done, time_to_sleep

    def reset_debug_ui(self):
        self.display_text.close()
        self.release_all_buttons()
        if self.is_rendering:
            self.is_rendering = False
            cv2.destroyWindow('window1')

    def reset(self):
        assert self.synced_timestamp is not None, "Cannot call env.reset() before sync time with game"

        self.elapsed_steps = 0
        self.last_step_time = time.time()
        obs = self.format_img_for_training(self.screenshot())

        # If this is before the first step of a quest, then fill frames with the same copies of current screenshot.
        # If this is before the first step of a episode within a quest, then skip this while loop,
        # and just append the current screenshot, after the last screenshots of the previous episode.
        self.frames.append(obs)
        while len(self.frames) < self.k:
            self.frames.append(obs)

        self.last_obs = np.array(self.frames)
        self.reset_debug_ui()

        self.episode_examples.clear()
        self.episode_examples.append({
            "end_time": self.last_step_time - self.synced_timestamp,
            "action": None,
            "next_obs": self.last_obs,
            "done": False,
        })

        return self.last_obs

    def get_examples(self):
        assert len(self.episode_examples) > 1, "Cannot call env.get_examples() and env.step() before calling reset()"
        last_time = time.time()
        result = []
        start_time = None
        obs = None
        episode_info = {
            "player_taken_damage": 0.,
            "enemy_taken_damage": 0.,
            "avgerage_distance": 0.,
            "episode_len": 0,
            "episode_reward": 0.,
            "average_step_reward": 0.,
        }

        self.judge.prepare_evaluation()
        for idx, example in enumerate(self.episode_examples):
            end_time = example["end_time"]
            action = example["action"]
            next_obs = example["next_obs"]
            done = example["done"]
            # The first example only provides the initial obs.
            if idx > 0:
                reward, game_info = self.judge.evaluate(start_time, end_time)
                if abs(reward) > abs(common_definitions.STEP_BASE_REWARD) * 1.1:
                    print("Step {}, reward {:.4f}".format(idx, reward))
                result.append((obs, action, next_obs, reward, done))

                (player_taken_damage, enemy_taken_damage, distance) = game_info
                episode_info["player_taken_damage"] += player_taken_damage
                episode_info["enemy_taken_damage"] += enemy_taken_damage
                episode_info["avgerage_distance"] += distance
                episode_info["episode_len"] += 1
                episode_info["episode_reward"] += reward

                if self.is_save_screenshot:
                    self.save_screenshot(idx, obs, action, reward, done, game_info)

            start_time = end_time
            obs = next_obs

        episode_info["average_step_reward"] = episode_info["episode_reward"] / episode_info["episode_len"]
        episode_info["avgerage_distance"] /= episode_info["episode_len"]

        print('evaluating reward took {:.3f} seconds, info: {}'.format(time.time()-last_time, episode_info))
        return result, episode_info

    def save_screenshot(self, step_idx, obs, action, reward, done, game_info):
        screenshot_outdir = os.path.join(self.outdir, 'quests', str(self.quest_idx), str(self.episode_idx))
        if not os.path.exists(screenshot_outdir):
            os.makedirs(screenshot_outdir)

        for idx, img in enumerate(obs):
            if idx == len(obs) - 1:
                # Wrtie info in the last one of the stacked frames.
                action_str = self.dqn_action_to_str(action)
                action_str = f'action:{action_str}'
                reward_str = f'reward:{reward}'
                done_str = f'done:{done}'
                (player_taken_damage, enemy_taken_damage, distance) = game_info
                info = f'p:{player_taken_damage},e:{enemy_taken_damage},d:{int(distance)}'
                self.display_text.add_text_to_img(img, [action_str, reward_str, done_str, info])

            file_path = os.path.join(screenshot_outdir, f"{step_idx}_{idx}.png")
            img = img * np.float32(255.)
            cv2.imwrite(file_path, img)

    def start_quest(self, wait_before_start=False):
        if (wait_before_start):
            self.operator.wait_before_start()

        self.judge.reset_is_quest_end()

        self.operator.start_quest_tetranadon()

        self.operator.sync_time_with_game()
        self.synced_timestamp = time.time()
        print(f'Synced time with game at {self.synced_timestamp}')

        time.sleep(5)
        self.operator.go_to_arena_center()

    def pause_game(self):
        self.operator.pause_game()

    def resume_game(self):
        self.episode_idx += 1
        self.operator.resume_game()

    def exit_quest(self):
        self.episode_idx = 0
        self.quest_idx += 1
        self.frames.clear()
        self.operator.exit_quest()

    def render(self):
        if self.last_obs is None:
            img = self.format_img_for_training(self.screenshot())
        else:
            img = self.last_obs[-1]
        img = cv2.resize(img, (self.img_width*2, self.img_height*2))
        cv2.imshow('window1', img)
        cv2.waitKey(1)
        self.is_rendering = True

    def close(self):
        pass

    def dqn_action_to_str(self, action):
        button_set = self.dqn_actions[action]
        msg = ''
        for button in button_set:
            msg += KEY_TO_STR[button] + ','
        return msg

    def dqn_perform_action(self, action):
        button_set = self.dqn_actions[action]
        for button in button_set:
            keys.PressKey(button)

        time.sleep(mhr_game_operator.MIN_KEY_PRESS_INTERVAL_SECONDS)
        for button in button_set:
            keys.ReleaseKey(button)

    def perform_action(self, action):
        msg = ''
        for idx, p in enumerate(action):
            button = self.buttons[idx]
            bstr = KEY_TO_STR[button]
            if p >= 0.5 and not self.button_states[idx]:
                ignored = True
                if button in self.enabled_buttons:
                    keys.PressKey(button)
                    ignored = False
                self.button_states[idx] = True
                # print('Pressed {}{}'.format(bstr, " (ignored)" if ignored else ''))
            elif p < 0.5 and self.button_states[idx]:
                keys.ReleaseKey(button)
                self.button_states[idx] = False
                # print('Released {}'.format(bstr))
            msg += "{}:{},".format(bstr, int(self.button_states[idx]))

        self.display_text.display(msg)

    def screenshot(self):
        need_grab_screen = True
        img = None
        while need_grab_screen:
            try:
                img = grabscreenv2.grab_screen("Monster Hunter Rise")
                need_grab_screen = False
            except RuntimeError as err:
                print(err)
                time.sleep(1)
        assert img is not None
        img = img[self.window_size[0]:self.window_size[1],
                  self.window_size[2]:self.window_size[3]]

        fi = self.elapsed_steps if self.save_img else -1
        if fi >= 0:
            fn = r'c:\temp\test\test_{}.png'.format(fi)
            cv2.imwrite(fn, img)
        return img

    def format_img_for_training(self, img):
        img = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        img = cv2.resize(img, (self.img_width, self.img_height))
        img = img / np.float32(255.)
        return img

    def release_all_buttons(self):
        msg = ''
        for idx, p in enumerate(self.buttons):
            button = self.buttons[idx]
            bstr = KEY_TO_STR[button]
            if self.button_states[idx]:
                keys.ReleaseKey(button)
                self.button_states[idx] = False
                # print('Released {}'.format(bstr))
            msg += "{}:{},".format(bstr, int(self.button_states[idx]))

        # self.display_text.display(msg)


def main():
    for i in list(range(1))[::-1]:
        print(i+1)
        time.sleep(1)

    env = MhEnv('')
    while True:
        img, damage = env.screenshot()
        cv2.imshow('window1', img)
        env.elapsed_steps += 1
        if cv2.waitKey(5) & 0xFF == ord('q'):
            break
    cv2.waitKey()  # 视频结束后，按任意键退出
    cv2.destroyAllWindows()


if __name__ == "__main__":
    main()
