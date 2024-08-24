#!/usr/bin/env bash

if [[ -f ~/.gitconfig && -f ~/.gitignore ]]; then
  info "git already configured!"
  return
fi

info "Configuring git"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/git/.gitconfig ~/.gitconfig
ln -sf ~/.dotfiles/files/git/.gitignore ~/.gitignore

info "Finished configuring git"