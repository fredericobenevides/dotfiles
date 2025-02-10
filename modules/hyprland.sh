#!/usr/bin/env bash

info "Configuring hyprland"

pacman "wayland"
pacman "waybar"
pacman "hyprland"
pacman "rofi-wayland"
pacman "grim"
pacman "slurp"
pacman "imv"
pacman "swayidle"
pacman "swaync"
yay "swaylock-effects-git"

if [[ -d ~/.config/hypr ]]; then
  info "hypr linking files already configured!"
  return
fi

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/hyprland ~/.config/hypr

echo -e "Finished configuring hyprland"
