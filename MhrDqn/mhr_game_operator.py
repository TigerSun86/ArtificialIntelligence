import directkeys as keys
import common_definitions
import time

KEY_PRESS_INTERVAL_SECONDS = 0.1
MOVE_INTERVAL_SECONDS = 0.2
MENU_LOADING_SECONDS = 2

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


class MhrGameOperator:
    def __init__(self) -> None:
        pass

    def wait_before_start(self):
        for i in range(5, 0, -1):
            print(i)
            time.sleep(1)

    def wait_for_game_loading(self):
        time.sleep(common_definitions.GAME_LOADING_SECONDS)

    def wait_for_menu_loading(self):
        time.sleep(MENU_LOADING_SECONDS)

    def wait_for_key_press(self):
        time.sleep(KEY_PRESS_INTERVAL_SECONDS)

    def wait_for_move(self):
        time.sleep(MOVE_INTERVAL_SECONDS)

    def open_map(self):
        keys.PressKey(KEY_OPEN_DETAILED_MAP)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_OPEN_DETAILED_MAP)
        self.wait_for_key_press()

    def open_start_menu(self):
        keys.PressKey(KEY_OPEN_START_MENU)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_OPEN_START_MENU)
        self.wait_for_key_press()

    def examine(self):
        keys.PressKey(KEY_EXAMINE)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_EXAMINE)
        self.wait_for_key_press()

    def cursor_up(self):
        keys.PressKey(KEY_MOVE_FORWARD)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_MOVE_FORWARD)
        self.wait_for_key_press()

    def cursor_down(self):
        keys.PressKey(KEY_MOVE_BACKWARDS)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_MOVE_BACKWARDS)
        self.wait_for_key_press()

    def cursor_left(self):
        keys.PressKey(KEY_MOVE_LEFT)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_MOVE_LEFT)
        self.wait_for_key_press()

    def cursor_right(self):
        keys.PressKey(KEY_MOVE_RIGHT)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_MOVE_RIGHT)
        self.wait_for_key_press()

    def move_forward(self):
        self.cursor_up()
        self.wait_for_move()

    def move_backward(self):
        self.cursor_down()
        self.wait_for_move()

    def move_left(self):
        self.cursor_left()
        self.wait_for_move()

    def move_right(self):
        self.cursor_right()
        self.wait_for_move()

    def wirebug_forward(self):
        keys.PressKey(KEY_WIREBUG_RETICLE)
        self.wait_for_key_press()
        keys.PressKey(KEY_SPECIAL_ATTACK)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_SPECIAL_ATTACK)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_WIREBUG_RETICLE)
        self.wait_for_key_press()

    def lock_on_target(self):
        keys.PressKey(KEY_LOCK_ON_TARGET)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_LOCK_ON_TARGET)
        self.wait_for_key_press()

    def go_to_arena_center(self):
        self.wirebug_forward()
        time.sleep(3)
        self.wirebug_forward()
        time.sleep(3)
        self.wirebug_forward()
        time.sleep(2)
        self.lock_on_target()

    def go_to_infernal_springs_center(self):
        self.wirebug_forward()
        time.sleep(3)
        self.move_right()
        self.wirebug_forward()
        time.sleep(3)
        self.move_left()
        self.move_left()
        self.examine()
        time.sleep(7)
        self.lock_on_target()
        time.sleep(2)
        self.move_forward()
        self.wirebug_forward()
        time.sleep(2)

    def launch_quest(self):
        keys.PressKey(KEY_DODGE)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_DODGE)
        self.wait_for_key_press()

        keys.PressKey(KEY_DODGE)
        self.wait_for_key_press()
        keys.ReleaseKey(KEY_DODGE)
        self.wait_for_key_press()

    def open_village_quest_menu(self):
        self.open_map()
        self.wait_for_menu_loading()
        self.examine()
        self.wait_for_menu_loading()

        for i in range(3):
            self.move_left()

        self.examine()
        self.wait_for_menu_loading()

    def open_master_rank_quest_menu(self):
        self.open_map()
        self.wait_for_menu_loading()
        self.examine()
        self.wait_for_menu_loading()

        for i in range(5):
            self.move_forward()

        for i in range(2):
            self.move_right()

        self.examine()
        self.wait_for_menu_loading()

    def setup_wroggi(self):
        self.open_village_quest_menu()

        self.examine()

        for i in range(3):
            self.cursor_up()

        self.examine()

        for i in range(4):
            self.cursor_down()

        self.examine()
        self.examine()
        self.wait_for_menu_loading()
        self.launch_quest()

        self.wait_for_game_loading()
        self.go_to_arena_center()

    def setup_tetranadon(self):
        self.open_master_rank_quest_menu()

        self.examine()
        self.cursor_down()
        self.examine()

        self.cursor_down()
        self.examine()
        self.wait_for_menu_loading()

        self.cursor_right()
        self.examine()
        self.examine()

        self.wait_for_menu_loading()
        self.launch_quest()

        self.wait_for_game_loading()
        self.go_to_arena_center()

    def setup_kuluyaku(self):
        self.open_master_rank_quest_menu()

        self.examine()
        self.cursor_down()
        self.examine()

        self.cursor_down()
        self.cursor_down()
        self.examine()
        self.wait_for_menu_loading()

        self.cursor_down()
        self.examine()
        self.examine()

        self.wait_for_menu_loading()
        self.launch_quest()

        self.wait_for_game_loading()
        self.go_to_infernal_springs_center()

    def exit_quest(self):
        time.sleep(15)

        self.cursor_up()
        self.examine()

        self.wait_for_menu_loading()

        self.examine()
        self.examine()

        self.wait_for_game_loading()

    def pause_game(self):
        self.open_start_menu()
        self.cursor_left()
        self.cursor_up()
        self.examine()

    def resume_game(self):
        self.open_start_menu()
