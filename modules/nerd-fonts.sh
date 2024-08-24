#!/usr/bin/env bash

if [ -d ~/.local/share/fonts/NerdFonts/JetBrainsMono ]; then
  info "Nerd Fonts already configured!"
  return
fi

info "Configuring Nerd Fonts in the system"

echo -e "-- Downloading JetBrainsMono"
curl -o /tmp/JetBrainsMono.tar.xz -L https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/JetBrainsMono.tar.xz

echo -e "-- Creating the Nerd Fonts folder"
mkdir -p ~/.local/share/fonts/NerdFonts/JetBrainsMono

echo -e "-- Extracting download file"
tar -xf /tmp/JetBrainsMono.tar.xz -C ~/.local/share/fonts/NerdFonts/JetBrainsMono

echo -e "-- Reloading font cache"
fc-cache

echo -e "-- Removing downloaded file"
rm /tmp/JetBrainsMono.tar.xz

info "Finished configuring Nerd Fonts in the system"