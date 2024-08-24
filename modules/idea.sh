#!/usr/bin/env bash

if [ -d ~/.idea ]; then
  info "idea already configured!"
  return
fi

info "Installing and configuring idea"

echo -e "-- Downloading intellij"
curl -o /tmp/idea -L https://download.jetbrains.com/idea/ideaIU-2023.1.7.tar.gz

echo -e "-- Creating the ~/.idea"
mkdir ~/.idea

echo -e "-- Extracting intellij to ~/.idea"
tar -C ~/.idea --strip-components=1 -xzvf /tmp/idea

echo -e "-- Removing download"
rm /tmp/idea

info "Finished installing and configuring idea"