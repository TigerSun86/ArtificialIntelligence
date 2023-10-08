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

    def evaluate(self, state):
        self.load_file_to_buffer()

        (state_timestamp, _) = state
        reward = 0
        while self.buffer:
            (log_timestamp, target, damage) = self.buffer[0]
            if state_timestamp >= log_timestamp:
                if target == "player":
                    reward -= damage
                else:
                    reward += damage
                self.buffer.popleft()
            else:
                # The log happened after the state, so it belongs to the next state.
                break
        reward /= 1000
        # reward += common_definitions.STEP_BASE_REWARD
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
            for event_timestamp, event in events:
                last_time = time.time()
                reward = judge.evaluate((event_timestamp, 0))
                print(
                    f"Event: {event} Event Timestamp: {datetime.fromtimestamp(event_timestamp).strftime('%Y-%m-%d %H:%M:%S')} Reward: {reward}")
                print('step took {:.3f} seconds'.format(
                    time.time()-last_time))
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
        (current_time - 30, "Event A"),
        (current_time, "Event B"),
        (current_time + 1000, "Event C")
    ]

    file_path = r"D:\Games\SteamLibrary\steamapps\common\MonsterHunterRise\reframework\data\shared_memory.txt"
    monitor_and_process(file_path, events_list)


if __name__ == "__main__":
    main()
