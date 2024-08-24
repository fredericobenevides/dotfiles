#!/usr/bin/env bash

if [ -f /swapfile ]; then
  info "swap already configured!"
  return
fi

info "Configuring swap in the system"

echo -e "-- Creating swap file"
sudo mkswap -U clear --size 64G --file /swapfile

echo -e "-- Activating the swap file"
sudo swapon /swapfile

echo -e "-- Adding swap file to /etc/fstab"
sudo echo '/swapfile none swap defaults 0 0' | sudo tee -a /etc/fstab

info "Finished configuring swap in the system"