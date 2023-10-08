import time
import cv2
import numpy as np


class DisplayText:
    def __init__(self):
        self.window_name = "Display Text Window"
        self.is_open = False

    def display(self, text):
        image = np.zeros((100, 1000, 3), dtype=np.uint8)

        font = cv2.FONT_HERSHEY_SIMPLEX
        font_scale = 1
        font_color = (255, 255, 255)
        line_type = cv2.LINE_AA

        text_size = cv2.getTextSize(text, font, font_scale, 1)[0]
        text_x = (image.shape[1] - text_size[0]) // 2
        text_y = (image.shape[0] + text_size[1]) // 2

        cv2.putText(image, text, (text_x, text_y), font,
                    font_scale, font_color, 1, line_type)

        cv2.imshow(self.window_name, image)
        cv2.waitKey(1)
        self.is_open = True

    def close(self):
        if self.is_open:
            cv2.destroyWindow(self.window_name)
            self.is_open = False


def main():
    display = DisplayText()
    display.display("Initial Text")

    time.sleep(1)
    display.display("Text 2")

    time.sleep(1)
    display.display("Text 3")

    # Close the window
    display.close()


if __name__ == "__main__":
    main()
