#!/usr/bin/env bash

if [ -d ~/.fzf ]; then
  info "fzf already configured!"
  return
fi

info "Installing and configuring fzf"

echo -e "-- Cloning fzf from git"
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf

echo -e "-- Running install script from fzf"
~/.fzf/install

info "Finished installing and configuring fzf"