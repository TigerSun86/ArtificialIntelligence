# -*- coding: utf-8 -*-
"""
Created on Wed Apr  8 10:37:50 2020

@author: analoganddigital   ( GitHub )
"""

import ctypes
import time

SendInput = ctypes.windll.user32.SendInput

KEY_ESC = 1
KEY_1 = 2
KEY_2 = 3
KEY_3 = 4
KEY_4 = 5
KEY_5 = 6
KEY_6 = 7
KEY_7 = 8
KEY_8 = 9
KEY_9 = 10
KEY_0 = 11
KEY_MINUS = 12
KEY_EQUAL = 13
KEY_BS = 14
KEY_TAB = 15
KEY_Q = 16
KEY_W = 17
KEY_E = 18
KEY_R = 19
KEY_T = 20
KEY_Y = 21
KEY_U = 22
KEY_I = 23
KEY_O = 24
KEY_P = 25
KEY_OPEN_BRACKET = 26
KEY_CLOSED_BRACKET = 27
KEY_ENTER = 28
KEY_CTRL = 29
KEY_A = 30
KEY_S = 31
KEY_D = 32
KEY_F = 33
KEY_G = 34
KEY_H = 35
KEY_J = 36
KEY_K = 37
KEY_L = 38
KEY_SEMICOLON = 39
KEY_QUOTE = 40
KEY_GRAVE = 41
KEY_LSHIFT = 42
KEY_BACKWARD_SLASH = 43
KEY_Z = 44
KEY_X = 45
KEY_C = 46
KEY_V = 47
KEY_B = 48
KEY_N = 49
KEY_M = 50
KEY_COMMA = 51
KEY_DOT = 52
KEY_FORWARD_SLASH = 53
KEY_RSHIFT = 54
KEY_PRTSC = 55
KEY_ALT = 56
KEY_SPACE = 57
KEY_CAPS = 58
KEY_F1 = 59
KEY_F2 = 60
KEY_F3 = 61
KEY_F4 = 62
KEY_F5 = 63
KEY_F6 = 64
KEY_F7 = 65
KEY_F8 = 66
KEY_F9 = 67
KEY_F10 = 68
KEY_NUM = 69
KEY_SCROLL = 70
KEY_HOME_7 = 71
KEY_UP_8 = 72
KEY_PGUP_9 = 73
KEY_GRAY_MINUS = 74
KEY_LEFT_4 = 75
KEY_CENTER_5 = 76
KEY_RIGHT_6 = 77
KEY_GRAY_PLUS = 78
KEY_END_1 = 79
KEY_DOWN_2 = 80
KEY_PGDN_3 = 81
KEY_INS = 82
KEY_DEL = 83


# C struct redefinitions
PUL = ctypes.POINTER(ctypes.c_ulong)


class KeyBdInput(ctypes.Structure):
    _fields_ = [("wVk", ctypes.c_ushort),
                ("wScan", ctypes.c_ushort),
                ("dwFlags", ctypes.c_ulong),
                ("time", ctypes.c_ulong),
                ("dwExtraInfo", PUL)]


class HardwareInput(ctypes.Structure):
    _fields_ = [("uMsg", ctypes.c_ulong),
                ("wParamL", ctypes.c_short),
                ("wParamH", ctypes.c_ushort)]


class MouseInput(ctypes.Structure):
    _fields_ = [("dx", ctypes.c_long),
                ("dy", ctypes.c_long),
                ("mouseData", ctypes.c_ulong),
                ("dwFlags", ctypes.c_ulong),
                ("time", ctypes.c_ulong),
                ("dwExtraInfo", PUL)]


class Input_I(ctypes.Union):
    _fields_ = [("ki", KeyBdInput),
                ("mi", MouseInput),
                ("hi", HardwareInput)]


class Input(ctypes.Structure):
    _fields_ = [("type", ctypes.c_ulong),
                ("ii", Input_I)]

# Actuals Functions


def PressKey(hexKeyCode):
    extra = ctypes.c_ulong(0)
    ii_ = Input_I()
    ii_.ki = KeyBdInput(0, hexKeyCode, 0x0008, 0, ctypes.pointer(extra))
    x = Input(ctypes.c_ulong(1), ii_)
    ctypes.windll.user32.SendInput(1, ctypes.pointer(x), ctypes.sizeof(x))


def ReleaseKey(hexKeyCode):
    extra = ctypes.c_ulong(0)
    ii_ = Input_I()
    ii_.ki = KeyBdInput(0, hexKeyCode, 0x0008 | 0x0002,
                        0, ctypes.pointer(extra))
    x = Input(ctypes.c_ulong(1), ii_)
    ctypes.windll.user32.SendInput(1, ctypes.pointer(x), ctypes.sizeof(x))


MOUSEEVENTF_MOVE = 0x0001
MOUSEEVENTF_LEFTDOWN = 0x0002
MOUSEEVENTF_LEFTUP = 0x0004


def LeftClick(hexKeyCode):
    extra = ctypes.c_ulong(0)
    ii_ = Input_I()
    ii_.mi = MouseInput(0, 0, 0, hexKeyCode, 0, ctypes.pointer(extra))
    x = Input(ctypes.c_ulong(1), ii_)
    ctypes.windll.user32.SendInput(1, ctypes.pointer(x), ctypes.sizeof(x))


def resharp():
    PressKey(KEY_E)
    time.sleep(0.2)
    ReleaseKey(KEY_E)
    time.sleep(1)
    PressKey(KEY_E)
    time.sleep(0.2)
    ReleaseKey(KEY_E)
    time.sleep(1)
    PressKey(KEY_E)
    time.sleep(0.2)
    ReleaseKey(KEY_E)
    time.sleep(10)


def attack():
    PressKey(KEY_B)
    time.sleep(1)
    ReleaseKey(KEY_B)
    time.sleep(1)


def go_forward():
    PressKey(KEY_W)
    time.sleep(1)
    ReleaseKey(KEY_W)


def dodge():
    PressKey(KEY_SPACE)
    time.sleep(0.1)
    ReleaseKey(KEY_SPACE)
    # time.sleep(0.1)


def lock_vision():
    PressKey(KEY_Q)
    time.sleep(0.1)
    ReleaseKey(KEY_Q)
    time.sleep(0.1)


if __name__ == '__main__':
    time.sleep(5)
    time1 = time.time()
    while (True):
        if abs(time.time()-time1) > 5:
            break
        else:
            PressKey(KEY_W)
            time.sleep(0.1)
            ReleaseKey(KEY_W)
            time.sleep(0.2)

    PressKey(KEY_W)
    time.sleep(0.4)
    ReleaseKey(KEY_W)
    time.sleep(1)
