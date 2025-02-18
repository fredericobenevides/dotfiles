#!/usr/bin/env bash

info "Configuring waybar"

if [[ -d ~/.config/waybar ]]; then
  info "waybar already configured!"
  return
fi

echo -e "-- Creating directory"
mkdir -p ~/.config/waybar

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/waybar/config ~/.config/waybar/config
ln -sf ~/.dotfiles/files/waybar/style.css ~/.config/waybar/style.css

info "Finished configuring waybar"
