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

function npm() {
    bash -c "npm -g list 2>&1 | grep $1 > /dev/null"
    if [ $? -eq 1 ]; then
      echo -e "-- Installing $1"
      bash -c "npm install -g $1"
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

function pip() {
    pip_cmd="~/.venv/bin/pip"
    bash -c "${pip_cmd} list | grep $1 > /dev/null 2>&1"
    if [ $? -eq 1 ]; then
        echo -e "-- Installing $1"
        bash -c "${pip_cmd} install $1"
    else
        echo -e "-- Package $1 is already installed"
    fi 

}
