import time

import cv2
import numpy as np
from gym import spaces

import directkeys as keys
import grabscreenv2
from display_text import DisplayText
import common_definitions

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
    def __init__(self, judge):
        self._elapsed_steps = 0

        self.save_img = False

        # self.buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT, KEY_MOVE_RIGHT, KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        # self.enabled_buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT, KEY_MOVE_RIGHT, KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        self.buttons = (KEY_MOVE_FORWARD, KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        self.enabled_buttons = (KEY_MOVE_FORWARD, KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        # self.buttons = (KEY_MOVE_FORWARD, KEY_MOVE_BACKWARDS, KEY_MOVE_LEFT, KEY_MOVE_RIGHT, KEY_DASH,
        #                 KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK, KEY_GUARD, KEY_WIREBUG_RETICLE, KEY_USE_ITEM)
        # self.enabled_buttons = (KEY_MOVE_FORWARD, KEY_MOVE_LEFT, KEY_MOVE_RIGHT,
        #                         KEY_DODGE, KEY_NORMAL_ATTACK, KEY_SPECIAL_ATTACK)
        self.button_states = np.full(len(self.buttons), False, dtype=np.bool8)

        self.dqn_actions = [{},
                            {KEY_MOVE_FORWARD},
                            {KEY_NORMAL_ATTACK},
                            {KEY_DODGE},
                            {KEY_MOVE_FORWARD, KEY_NORMAL_ATTACK},
                            ]

        self.need_wait_between_steps = True
        self.time_between_steps = 0.2
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
        self.img_width = 84
        self.img_height = 84
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

        self.screenshot_for_render = None

        self.judge = judge

        self.kd = DisplayText()

    def get_action_number(self):
        return self.action_num

    def step_without_reward(self, action):
        assert self._elapsed_steps is not None, "Cannot call env.step() before calling reset()"

        # self.perform_action(action)
        self.dqn_perform_action(action)

        self._elapsed_steps += 1

        if (self.need_wait_between_steps):
            cur_time = time.time()
            difference = cur_time - self.last_step_time
            time_to_sleep = self.time_between_steps - difference
            if (time_to_sleep > 0):
                time.sleep(time_to_sleep)

        self.judge.load_file_to_buffer()

        self.last_step_time = time.time()
        self.screenshot_for_render = self.screenshot()
        observation = (time.time(), self.screenshot_for_render)

        return observation, 0, self.judge.is_quest_end, 0

    def step(self, action):
        observation, _, done, _ = self.step_without_reward(action)
        damage = self.evaluate_reward(observation)

        return observation, damage, done, 0

    def evaluate_reward(self, observation):
        damage = self.judge.evaluate(observation)
        return damage

    def exit_quest(self):
        time.sleep(15)
        keys.PressKey(KEY_MOVE_FORWARD)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_FORWARD)
        time.sleep(0.2)
        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(10)

    def reset(self):
        self._elapsed_steps = 0
        self.last_step_time = time.time()

        self.release_all_buttons()

        # self.setup_train()

        observation = (time.time(), self.screenshot())
        return observation

    def pause_game(self):
        keys.PressKey(KEY_OPEN_START_MENU)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_OPEN_START_MENU)
        time.sleep(0.2)
        keys.PressKey(KEY_MOVE_LEFT)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_LEFT)
        time.sleep(0.2)
        keys.PressKey(KEY_MOVE_FORWARD)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_FORWARD)
        time.sleep(0.2)
        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

    def resume_game(self):
        keys.PressKey(KEY_OPEN_START_MENU)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_OPEN_START_MENU)
        time.sleep(0.2)

    def setup_wroggi(self):
        self.judge.reset_is_quest_end()

        time.sleep(4)

        keys.PressKey(KEY_OPEN_DETAILED_MAP)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_OPEN_DETAILED_MAP)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        for i in range(2):
            keys.PressKey(KEY_MOVE_LEFT)
            time.sleep(0.2)
            keys.ReleaseKey(KEY_MOVE_LEFT)
            time.sleep(0.5)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        for i in range(3):
            keys.PressKey(KEY_MOVE_FORWARD)
            time.sleep(0.2)
            keys.ReleaseKey(KEY_MOVE_FORWARD)
            time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        for i in range(4):
            keys.PressKey(KEY_MOVE_BACKWARDS)
            time.sleep(0.2)
            keys.ReleaseKey(KEY_MOVE_BACKWARDS)
            time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_DODGE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_DODGE)
        time.sleep(0.2)

        keys.PressKey(KEY_DODGE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_DODGE)
        time.sleep(0.2)

        time.sleep(10)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(3)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_LOCK_ON_TARGET)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_LOCK_ON_TARGET)
        time.sleep(0.2)

        time.sleep(2)

    def setup_tetranadon(self):
        self.judge.reset_is_quest_end()

        time.sleep(4)

        keys.PressKey(KEY_OPEN_DETAILED_MAP)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_OPEN_DETAILED_MAP)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        for i in range(5):
            keys.PressKey(KEY_MOVE_FORWARD)
            time.sleep(0.2)
            keys.ReleaseKey(KEY_MOVE_FORWARD)
            time.sleep(0.5)

        for i in range(2):
            keys.PressKey(KEY_MOVE_RIGHT)
            time.sleep(0.2)
            keys.ReleaseKey(KEY_MOVE_RIGHT)
            time.sleep(0.5)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        keys.PressKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        keys.PressKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_MOVE_RIGHT)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_RIGHT)
        time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_DODGE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_DODGE)
        time.sleep(0.2)

        keys.PressKey(KEY_DODGE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_DODGE)
        time.sleep(0.2)

        time.sleep(10)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(10)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_LOCK_ON_TARGET)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_LOCK_ON_TARGET)
        time.sleep(0.2)

        time.sleep(2)

    def setup_train(self):
        keys.PressKey(KEY_OPEN_DETAILED_MAP)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_OPEN_DETAILED_MAP)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_MOVE_BACKWARDS)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_EXAMINE)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_EXAMINE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)
        keys.PressKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        time.sleep(0.2)

        time.sleep(2)

        keys.PressKey(KEY_LOCK_ON_TARGET)
        time.sleep(0.2)
        keys.ReleaseKey(KEY_LOCK_ON_TARGET)
        time.sleep(0.2)

        time.sleep(2)

    def render(self):
        if hasattr(self.screenshot_for_render, "__len__") and len(self.screenshot_for_render) > 0:
            img = cv2.cvtColor(self.screenshot_for_render, cv2.COLOR_BGR2GRAY)
            img = cv2.resize(img, (self.img_width, self.img_height))
            img = cv2.resize(img, (self.img_width*5, self.img_height*5))
            cv2.imshow('window1', img)

    def close(self):
        pass

    def dqn_perform_action(self, action):
        button_set = self.dqn_actions[action]
        button_nums = [1 if button in button_set else 0 for button in self.buttons]
        self.perform_action(button_nums)

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

        self.kd.display(msg)

    def screenshot(self):
        need_grab_screen = True
        while need_grab_screen:
            try:
                img = grabscreenv2.grab_screen("Monster Hunter Rise")
                need_grab_screen = False
            except RuntimeError as err:
                print(err)
                time.sleep(1)
        img = img[self.window_size[0]:self.window_size[1],
                  self.window_size[2]:self.window_size[3]]

        fi = self._elapsed_steps if self.save_img else -1
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

        self.kd.close()


def main():
    for i in list(range(1))[::-1]:
        print(i+1)
        time.sleep(1)

    env = MhEnv()
    while True:
        img, damage = env.screenshot()
        cv2.imshow('window1', img)
        env._elapsed_steps += 1
        if cv2.waitKey(5) & 0xFF == ord('q'):
            break
    cv2.waitKey()  # 视频结束后，按任意键退出
    cv2.destroyAllWindows()


if __name__ == "__main__":
    main()
