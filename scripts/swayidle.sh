#!/usr/bin/env bash

command="swaylock -f \
                  --ignore-empty-password \
                  --clock \
                  --image /usr/share/hypr/wall2.png"

swayidle -w \
	 timeout 300 "${command}" \
         timeout 600 'hyprctl dispatch dpms off' \
         resume 'hyprctl dispatch dpms on' \
         before-sleep "${command}"
