#!/bin/bash

current=$(cat /sys/class/backlight/intel_backlight/brightness)
echo $(($current + 1000)) > /sys/class/backlight/intel_backlight/brightness

