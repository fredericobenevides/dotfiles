#!/bin/bash

set -e

install_vim_plug() {
  echo "Installing the plugin vim-plug to manage the vim plugins"
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

install_neovim_plug() {
  echo "Installing the plugin vim-plug to manage the vim plugins"
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
}

install_matchit_plugin() {
  echo "Installing the matchit plugin"
  mkdir -p ~/.vim/plugin
  vim -e --cmd 'exe "set t_cm=\<C-M>"|!cp $VIMRUNTIME/macros/matchit.vim ~/.vim/plugin' +visual +qall

  mkdir -p ~/.vim/doc
  vim -e --cmd 'exe "set t_cm=\<C-M>"|!cp $VIMRUNTIME/macros/matchit.txt ~/.vim/doc' +visual +qall
  vim -e --cmd 'exe "set t_cm=\<C-M>"|helptags ~/.vim/doc' +visual +qall
}

launch_vim_to_install_all_plugins() {
  echo "Launching vim to install all plugins"
  vim +PlugInstall +qall
}

launch_neovim_to_install_all_plugins() {
  echo "Launching neovim to install all plugins"
  nvim +PlugInstall +qall
}

setup_vim() {
  install_matchit_plugin

  install_vim_plug
  launch_vim_to_install_all_plugins
}

setup_neovim() {
  pip install neovim

  mkdir -p $HOME/.config/nvim

  nvim=`which nvim`
  if [ ! -n $nvim ]; then
    install_neovim_plug
    launch_neovim_to_install_all_plugins
  fi
}

setup_neovim
