#!/bin/bash

set -e

kernel=5.1.16-1
header=51

sudo mhwd -i pci video-nvidia

sudo pacman -S linux${header}-headers acpi_call-dkms xorg-xrandr xf86-video-intel

sudo modprobe acpi_call

git clone https://github.com/dglt1/optimus-switch-sddm.git ~/.optimus-switch-sddm
cd ~/.optimus-switch-sddm

chmod +x install.sh
sudo ./install.sh

