#!/usr/bin/bash

if [ -d ~/.flutter ]; then
  info "flutter already configured!"
  return
fi

info "Installing and configuring flutter"

echo -e "-- Downloading flutter"
curl -o /tmp/flutter -L https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_3.24.1-stable.tar.xz

echo -e "-- Creating the ~/.flutter"
mkdir ~/.flutter

echo -e "-- Extracting flutter to ~/.flutter"
tar -C ~/.flutter -xvf /tmp/flutter

info "Finished installing and configuring flutter"