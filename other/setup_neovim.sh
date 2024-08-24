#!/usr/bin/env bash

. ../modules/utils.sh

info "Installing and configuring neovim"

pacman "neovim"

echo -e "-- Creating the folder ~/.config/nvim"
mkdir -p ~/.config/nvim

echo -e "-- Creating the folder ~/.vim"
mkdir -p ~/.vim

echo -e "-- Creating the folder ~/.local/share/nvim/site/autoload"
mkdir -p ~/.local/share/nvim/site/autoload

echo -e "-- Installing vim-plug"
curl -o ~/.local/share/nvim/site/autoload/plug.vim -L https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo -e "-- Creating themes folder"
mkdir -p ~/.local/share/nvim/site/pack/themes/start

echo -e "-- Linking files"
ln -sf ~/.dotfiles/files/vim/.vimrc ~/.vimrc
ln -sf ~/.dotfiles/files/vim/.vimrc ~/.config/nvim/init.vim
ln -sf ~/.dotfiles/files/vim/coc-settings.json ~/.config/nvim/coc-settings.json
ln -sf ~/.dotfiles/files/themes/vim ~/.local/share/nvim/site/pack/themes/start/dracula_pro

info "Finished installing and configuring neovim"