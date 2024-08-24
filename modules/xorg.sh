#!/usr/bin/env bash

if [ -f ~/.XCompose ]; then
  info "xorg already configured!"
  return
fi

info "Installing and configuring xorg"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/xorg/.conkyrc ~/.conkyrc
ln -sf ~/.dotfiles/files/xorg/.XCompose ~/.XCompose

#ln -sf ~/.dotfiles/files/xorg/.xbindkeysrc ~/.xbindkeysrc
#ln -sf ~/.dotfiles/files/xorg/.xprofile ~/.xprofile

info "Finished installing and configuring xorg"