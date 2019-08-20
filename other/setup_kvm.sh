#!/bin/bash

sudo pacman -S virt-manager qemu vde2 ebtables dnsmasq bridge-utils openbsd-netcat

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service
