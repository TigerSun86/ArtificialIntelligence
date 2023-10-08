
import cv2
import numpy as np
from ctypes import windll
import win32gui
import win32ui
from time import time


def grab_screen(window_name: str):
    # Adapted from https://stackoverflow.com/questions/19695214/screenshot-of-inactive-window-printwindow-win32gui

    windll.user32.SetProcessDPIAware()
    hwnd = win32gui.FindWindow(None, window_name)
    if hwnd == 0:
        raise Exception("Game window '{}' not found.".format(window_name))

    left, top, right, bottom = win32gui.GetClientRect(hwnd)
    w = right - left
    h = bottom - top

    hwnd_dc = win32gui.GetWindowDC(hwnd)
    mfc_dc = win32ui.CreateDCFromHandle(hwnd_dc)
    save_dc = mfc_dc.CreateCompatibleDC()
    bitmap = win32ui.CreateBitmap()
    bitmap.CreateCompatibleBitmap(mfc_dc, w, h)
    save_dc.SelectObject(bitmap)

    # If Special K is running, this number is 3. If not, 1
    result = windll.user32.PrintWindow(hwnd, save_dc.GetSafeHdc(), 3)

    bmpinfo = bitmap.GetInfo()
    bmpstr = bitmap.GetBitmapBits(True)

    img = np.frombuffer(bmpstr, dtype=np.uint8).reshape(
        (bmpinfo["bmHeight"], bmpinfo["bmWidth"], 4))
    # make image C_CONTIGUOUS and drop alpha channel
    img = np.ascontiguousarray(img)[..., :-1]

    win32gui.DeleteObject(bitmap.GetHandle())
    save_dc.DeleteDC()
    mfc_dc.DeleteDC()
    win32gui.ReleaseDC(hwnd, hwnd_dc)

    if not result:  # result should be 1
        raise RuntimeError(f"Unable to acquire screenshot! Result: {result}")

    return img


def main():

    loop_time = time()
    WINDOW_NAME = "Monster Hunter Rise"
    while cv2.waitKey(1) != ord('q'):
        screenshot = grab_screen(WINDOW_NAME)
        cv2.imshow('Computer Vision', screenshot)
        # debug the loop rate
        print('FPS {}'.format((time() - loop_time)))
        loop_time = time()


if __name__ == '__main__':
    main()
