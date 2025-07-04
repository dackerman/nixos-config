#!/usr/bin/env bash

# Wake up HDMI monitor
# First force DPMS on, then ensure the monitor is enabled
DISPLAY=:0 xset dpms force on
DISPLAY=:0 xrandr --output HDMI-0 --auto --primary