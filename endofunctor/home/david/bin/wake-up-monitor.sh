#!/usr/bin/env bash

# Wake up HDMI monitor and position it to the right of DP-2
DISPLAY=:0 xrandr --output HDMI-0 --auto --right-of DP-2