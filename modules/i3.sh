#!/usr/bin/env bash

ls -l ~/.config/i3/config 2>&1 | grep dotfiles > /dev/null
if [ $? -eq 0 ]; then
  info "i3 already configured!"
  return
fi

info "Configuring i3"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/i3/config ~/.config/i3/config

info "Finished configuring i3"