#!/usr/bin/env bash

groups 2>&1 | grep docker > /dev/null
if [ $? -eq 0 ]; then
  info "docker already configured!"
  return
fi

info "Installing and configuring docker"

pacman "docker"
pacman "docker-compose"

echo -e "-- Adding and enabling docker.service to systemctl"
sudo systemctl start docker.service
sudo systemctl enable docker.service

echo -e "-- Add the current user to the group docker"

sudo usermod -aG docker "$USER"
newgrp docker

info "Finished installing and configuring docker"