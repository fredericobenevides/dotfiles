#!/bin/bash

set -e

kernel_version=`uname -a | grep Linux | awk '{print $3}' | sed -E 's/^([0-9]+)\.([0-9]+).*/\1\2/g'`

sudo mhwd -i pci video-nvidia

sudo pacman -S linux${kernel_version}-headers acpi_call-dkms xorg-xrandr xf86-video-intel

sudo modprobe acpi_call

git clone https://github.com/dglt1/optimus-switch-sddm.git ~/.optimus-switch-sddm
cd ~/.optimus-switch-sddm

chmod +x install.sh
sudo ./install.sh

