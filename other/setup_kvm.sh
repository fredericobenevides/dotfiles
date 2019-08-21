#!/bin/bash

sudo pacman -S virt-manager qemu vde2 ebtables dnsmasq bridge-utils openbsd-netcat

sudo systemctl enable libvirtd.service
sudo systemctl start libvirtd.service

sudo usermod -a -G kvm,libvirt $(whoami)

newgrp libvirt
newgrp kvm

curl -L https://github.com/dhiltgen/docker-machine-kvm/releases/download/v0.10.0/docker-machine-driver-kvm-centos7 -o /tmp/docker-machine-driver-kvm
sudo mv /tmp/docker-machine-driver-kvm /usr/local/bin
sudo chmod +x /usr/local/bin/docker-machine-driver-kvm

sudo virsh net-start default
sudo virsh net-autostart default
