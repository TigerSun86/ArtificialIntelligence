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
        self.is_quest_end = False

    def reset_is_quest_end(self):
        self.is_quest_end = False

    def evaluate(self, start_time, end_time):
        self.load_file_to_buffer()
        # print(f'start_time {start_time} end_time {end_time}')
        reward = 0
        while self.buffer:
            (log_timestamp, target, damage) = self.buffer[0]
            # print(f'log_timestamp {log_timestamp} target {target} damage {damage}')
            if log_timestamp > end_time:
                # The log happened after the state, so it belongs to the next state.
                break
            if log_timestamp > start_time:
                if target == "player":
                    reward -= damage
                else:
                    reward += damage
            self.buffer.popleft()

        # print(f'reward {reward}')
        reward /= 10.
        reward += common_definitions.STEP_BASE_REWARD
        return reward

    def load_file_to_buffer(self):
        with open(self.game_log_path, "r") as file:
            line = file.readline()
            if not line:
                raise Exception(f"Game log file {self.game_log_path} is not expected to be empty.")

            # If this is a new file, reset counters.
            first_line = line.strip().split()
            assert first_line[1] == "file_create_time", "The first line of log file is expected to be a timestamp and a string 'file_create_time'"
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
                    log_timestamp = log[0]
                    text = log[1]
                    if text == "end":
                        self.is_quest_end = True
                        break
                    elif len(log) == 3:
                        damage = log[2]
                        self.buffer.append((int(log_timestamp), text, int(damage)))

# Test


def monitor_and_process(file_path, events):
    if not events or len(events) == 0:
        return

    judge = ModRewardJudge(file_path)

    while True:
        try:
            i = 0
            for start_time, end_time in events:
                last_time = time.time()
                reward = judge.evaluate(start_time, end_time)
                print(
                    f"Event: {i} Event Timestamp: {datetime.fromtimestamp(start_time).strftime('%Y-%m-%d %H:%M:%S')} Reward: {reward}")
                print('step took {:.3f} seconds'.format(
                    time.time()-last_time))
                i += 1
        except KeyboardInterrupt:
            break
        # except Exception as e:
        #     print("Error:", e)

        # Wait for a short interval before checking again
        time.sleep(1)


def main():
    # Simulated list of events with timestamps
    current_time = int(time.time())
    events_list = [
        (current_time - 30, current_time - 10),
        (current_time - 10, current_time),
        (current_time, current_time + 1000)
    ]

    file_path = common_definitions.GAME_LOG_PATH
    monitor_and_process(file_path, events_list)


if __name__ == "__main__":
    main()
