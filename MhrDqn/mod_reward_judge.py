from collections import deque
from datetime import datetime
import time
import common_definitions


class ModRewardJudge:

    def __init__(self, game_log_path):
        self.game_log_path = game_log_path
        self.game_log_create_time = 0
        self.last_position = 0
        self.buffer = deque()
        self.time_and_distances = deque()
        self.is_quest_end = False
        self.cur_time_and_distance = (0, 0)

    def reset_is_quest_end(self):
        self.is_quest_end = False

    def prepare_evaluation(self):
        self.load_file_to_buffer()
        self.load_time_and_distances()

    def evaluate(self, start_time, end_time):
        # print(f'start_time {start_time} end_time {end_time}')
        reward = 0
        player_taken_damage = 0
        enemy_taken_damage = 0
        player_taken_damage_count = 0
        enemy_taken_damage_count = 0
        while self.buffer:
            (log_elapsed_time, target, damage) = self.buffer[0]
            # print(f'log_elapsed_time {log_elapsed_time} target {target} damage {damage}')
            if log_elapsed_time > end_time:
                # The log happened after the state, so it belongs to the next state.
                break
            if log_elapsed_time > start_time:
                if target == "player":
                    player_taken_damage = damage
                    player_taken_damage_count += 1
                elif target == "enemy":
                    enemy_taken_damage = damage
                    enemy_taken_damage_count += 1
            self.buffer.popleft()

        # print(f'player_taken_damage {player_taken_damage}')
        # print(f'enemy_taken_damage {enemy_taken_damage}')
        reward = (enemy_taken_damage - player_taken_damage) / 1000.

        close_distance_count = 0
        far_distance_count = 0
        distance = self.get_distance(end_time)
        # print(f'distance {distance}')
        if distance <= 10:
            reward += common_definitions.STEP_BASE_REWARD
            close_distance_count += 1
        elif distance > 20:
            reward -= common_definitions.STEP_BASE_REWARD
            far_distance_count += 1

        return reward, (player_taken_damage, enemy_taken_damage, distance, player_taken_damage_count,
                        enemy_taken_damage_count, close_distance_count, far_distance_count)

    def get_distance(self, action_time):
        while self.time_and_distances and action_time > self.time_and_distances[0][0]:
            self.cur_time_and_distance = self.time_and_distances[0]
            # Pop all items happened before the action time.
            # In practice, only one pop is enough because the action is much more frequent than the distance logging.
            self.time_and_distances.popleft()

        if not self.time_and_distances:
            # The action happened after the last logged distance, just use the last distance.
            return self.cur_time_and_distance[1]

        # The action time is between the cur and next time.
        # Calculate the distance between the cur and next distance.
        cur_time, cur_distance = self.cur_time_and_distance
        next_time, next_distance = self.time_and_distances[0]
        distance = cur_distance + (next_distance - cur_distance)*(action_time - cur_time)/(next_time - cur_time)

        return distance

    def load_time_and_distances(self):
        self.cur_time_and_distance = (0, 0)
        self.time_and_distances.clear()
        for log_elapsed_time, category, data in self.buffer:
            if category == "distance":
                self.time_and_distances.append((log_elapsed_time, data))

    def load_file_to_buffer(self):
        with open(self.game_log_path, "r") as file:
            line = file.readline()
            if not line:
                raise Exception(f"Game log file {self.game_log_path} is not expected to be empty.")

            # If this is a new file, reset counters.
            first_line = line.strip().split()
            assert first_line[1] == "time_synced", "The first line of log file is expected to be a timestamp and a string 'time_synced'"
            game_log_create_time = int(first_line[0])
            if self.game_log_create_time != game_log_create_time:
                self.game_log_create_time = game_log_create_time
                self.last_position = file.tell()
                self.buffer.clear()

            file.seek(self.last_position)
            lines = file.readlines()
            if lines:
                self.last_position = file.tell()
                for line in lines:
                    log = line.strip().split()
                    log_elapsed_time = log[0]
                    text = log[1]
                    if text == "end":
                        self.is_quest_end = True
                        break
                    elif len(log) == 3:
                        damage = log[2]
                        self.buffer.append((float(log_elapsed_time), text, int(damage)))

# Test


def monitor_and_process(file_path, events):
    if not events or len(events) == 0:
        return

    judge = ModRewardJudge(file_path)

    i = 0
    for start_time, end_time in events:
        last_time = time.time()
        reward = judge.evaluate(start_time, end_time)
        print(
            f"Event: {i} start_time: {start_time} end_time: {end_time} Reward: {reward}")
        print('step took {:.3f} seconds'.format(
            time.time()-last_time))
        i += 1


def main():
    # Simulated list of events with timestamps
    events_list = [
        (20.1, 30.1),
        (30.1, 40.1),
        (40.1, 70.1),
    ]

    file_path = common_definitions.GAME_LOG_PATH
    monitor_and_process(file_path, events_list)


if __name__ == "__main__":
    main()
