#!/bin/bash

install_neovim_app() {
  nvim=`which nvim`

  if [ -z $nvim ]; then
    sudo mkdir -p /opt/neovim
    curl -L https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage -o /tmp/nvim.appimage
    sudo mv /tmp/nvim.appimage /opt/neovim
    sudo chmod u+x /opt/neovim/nvim.appimage
  fi
}

install_neovim_app
