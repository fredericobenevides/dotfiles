#!/usr/bin/env bash

info "Configuring hyprland"

pacman "wayland"
pacman "hyprland"
pacman "rofi-wayland"
pacman "grim"
pacman "slurp"
pacman "imv"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/hyprland/hyprland.conf ~/.config/hypr/hyprland.conf

echo -e "Finished configuring hyprland"
