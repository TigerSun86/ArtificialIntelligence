import time
import cv2
import numpy as np


class DisplayText:
    def __init__(self):
        self.window_name = "Display Text Window"
        self.is_open = False
        self.font = cv2.FONT_HERSHEY_SIMPLEX
        self.font_scale = 0.4
        self.font_color = (255, 255, 255)
        self.line_type = cv2.LINE_AA

        self.init_x = 2
        self.init_y = 10

        text_size = cv2.getTextSize("aaa", self.font, self.font_scale, 1)[0]
        text_height = text_size[1]
        self.y_offset = text_height

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

    def add_text_to_img(self, img, texts: list):
        if len(texts) == 0:
            return img

        text_x = self.init_x
        text_y = self.init_y

        for text in texts:
            cv2.putText(img, text, (text_x, text_y), self.font,
                        self.font_scale, self.font_color, 1, self.line_type)
            text_y += self.y_offset

        return img


def main():
    display = DisplayText()
    import grabscreenv2
    img = grabscreenv2.grab_screen("test_0.png ‎- Photos")
    img = cv2.cvtColor(np.float32(img), cv2.COLOR_RGB2GRAY)
    img = cv2.resize(img, (180, 180))
    img = img / np.float32(255.)
    result = []
    result.append("action: W, SPACE")
    result.append("reward: 0.1234")
    result.append("done: False")
    img = display.add_text_to_img(img, result)

    cv2.imshow("window_name", img)
    cv2.waitKey(1)

    img = img * np.float32(255.)
    cv2.imwrite('c:\\temp\\test.png', img)
    time.sleep(100)

    cv2.destroyWindow("window_name")


if __name__ == "__main__":
    main()
