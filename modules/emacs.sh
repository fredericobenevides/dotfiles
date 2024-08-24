#!/usr/bin/env bash

if [ -d ~/.emacs.d ]; then
  info "emacs already configured!"
  return
fi

info "Installing and configuring emacs"

pacman "emacs"

echo -e "-- Creating the folder ~/.emacs.d"
mkdir -p ~/.emacs.d

echo -e "-- Linking files inside ~/.emacs.d"
ln -sf ~/.dotfiles/files/emacs/early-init.el ~/.emacs.d/early-init.el
ln -sf ~/.dotfiles/files/emacs/init.el ~/.emacs.d/init.el
ln -sf ~/.dotfiles/files/emacs/init.org ~/.emacs.d/init.org

info "Finished installing and configuring emacs"