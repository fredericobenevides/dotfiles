#!/usr/bin/env bash

info "Configuring hyprland"

pacman "wayland"
pacman "waybar"
pacman "hyprland"
pacman "rofi-wayland"
pacman "grim"
pacman "slurp"
pacman "imv"
yay -S "swaylock-effects-git"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/hyprland ~/.config/hypr

echo -e "Finished configuring hyprland"
