#!/usr/bin/env bash

if [ -f ~/.XCompose ]; then
  info "x11 already configured!"
  return
fi

info "Installing and configuring x11"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/x11/.XCompose ~/.XCompose

info "Finished installing and configuring x11"
