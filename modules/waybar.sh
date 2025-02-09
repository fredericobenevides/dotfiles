#!/usr/bin/env bash

info "Configuring waybar"

if [[ -d ~/.config/waybar ]]; then
  info "waybar already configured!"
  return
fi

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/waybar ~/.config/waybar

info "Finished configuring waybar"
