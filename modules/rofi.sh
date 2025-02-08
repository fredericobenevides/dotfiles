#!/usr/bin/env bash

info "Configuring rofi"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/rofi/rofidmenu.rasi ~/.config/rofi/rofidmenu.rasi 

info "Finished configuring rofi"
