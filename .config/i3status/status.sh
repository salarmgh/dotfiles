#!/bin/bash

i3status | while :
do
    read line
    mem_free=$(echo "`cat /proc/meminfo | grep "MemFree" | awk '{print $2}'` / 1024 / 1024" | bc)
    uptime=$(uptime | cut -d' ' -f2)
    echo "Uptime: $uptime | Mem: $mem_free""G | $line" || exit 1
done
