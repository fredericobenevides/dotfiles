#!/usr/bin/env bash

function install_flatpak() {
    /usr/bin/pacman -Qi flatpak > /dev/null 2>&1
    if [ $? -eq 0 ]; then
      info "flatpak already configured!"
      return
    fi

  info "Installing and configuring flatkpak"

  pacman "flatpak"

  echo -e "-- Adding the Flathub repository"
  flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

  info "Finished installing and configuring flatkpak"
}

function install_pkgs_flatpak() {
  info "Installing flatkpak packages"

  flatpak "com.google.AndroidStudio"
  flatpak "com.spotify.Client"
  flatpak "rest.insomnia.Insomnia"

  info "Finished installing flatkpak packages"
}

install_flatpak
install_pkgs_flatpak
