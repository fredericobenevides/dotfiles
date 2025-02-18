#!/usr/bin/env bash

info "Configuring rofi"

if [[ -d ~/.config/rofi ]]; then
  info "rofi already configured!"
  return
fi

echo -e "-- Creating directory"
mkdir -p ~/.config/rofi

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/rofi/config.rasi ~/.config/rofi/config.rasi
ln -sf ~/.dotfiles/files/rofi/power-menu.rasi ~/.config/rofi/power-menu.rasi
ln -sf ~/.dotfiles/files/rofi/power-profiles.rasi ~/.config/rofi/power-profiles.rasi
ln -sf ~/.dotfiles/files/rofi/rofikeyhint.rasi ~/.config/rofi/rofikeyhint.rasi
ln -sf ~/.dotfiles/files/rofi/rofidmenu.rasi ~/.config/rofi/rofidmenu.rasi

info "Finished configuring rofi"
