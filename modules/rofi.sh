#!/usr/bin/env bash

info "Configuring rofi"

if [[ -d ~/.config/rofi ]]; then
  info "rofi already configured!"
  return
fi

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/rofi ~/.config/rofi

info "Finished configuring rofi"
