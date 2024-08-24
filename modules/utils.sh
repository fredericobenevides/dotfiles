#!/usr/bin/env bash

function info() {
  echo -e "#### $1"
}

function flatpak() {
  /usr/bin/flatpak list 2>&1 | grep $1 > /dev/null
  if [ $? -eq 1 ]; then
    echo -e "-- Installing $1"
    sudo bash -c "flatpak install flathub $1"
  else
    echo -e "-- Package $1 is already installed"
  fi
}

function pacman() {
  /usr/bin/pacman -Qi $1 > /dev/null 2>&1
  if [ $? -eq 1 ]; then
    echo -e "-- Installing $1"
    sudo bash -c "pacman -S $1"
  else
    echo -e "-- Package $1 is already installed"
  fi
}