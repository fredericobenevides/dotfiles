#!/usr/bin/env bash

if [ -d $HOME/.local/share/applications ]; then
  info "idea already configured!"
  return
fi

info "Installing and configuring desktop files"

echo -e "-- Creating desktop application's folder"
mkdir -p $HOME/.local/share/applications

echo -e "-- Copying desktop files"
cp $HOME/.dotfiles/files/desktop/idea.desktop $HOME/.local/share/applications/idea.desktop

echo -e "-- Updating the right username in desktop file"
sed -i "s|{{USER}}|$USER|" ~/.local/share/applications/idea.desktop

info "Finished installing and configuring desktop files"
