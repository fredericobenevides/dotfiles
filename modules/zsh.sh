#!/usr/bin/env bash

if [ -d ~/.oh-my-zsh ]; then
  info "zsh already configured!"
  return
fi

info "Installing and configuring zsh"

echo -e "-- Download and installing oh-my-zsh"
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/zsh/zshrc ~/.zshrc
ln -sf ~/.dotfiles/files/zsh/plugins/fredericobenevides ~/.oh-my-zsh/custom/plugins/fredericobenevides
ln -sf ~/.dotfiles/files/zsh/plugins/myutils ~/.oh-my-zsh/custom/plugins/myutils
ln -sf ~/.dotfiles/files/zsh/themes/fredericobenevides.zsh-theme ~/.oh-my-zsh/themes/fredericobenevides.zsh-theme

info "Finished installing and configuring zsh"