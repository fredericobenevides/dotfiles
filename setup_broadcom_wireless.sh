#!/bin/bash

# script based on:
# https://forum.manjaro.org/t/broadcom-wifi-driver-fails-to-install-properly/46316/2

sudo pacman -S linux$(uname -r| grep -o -E '[0-9]+' | head -n 2 | sed 'N;s/\n//')-headers

sudo pacman -S dkms

sudo pacman -S broadcom-wl-dkms
