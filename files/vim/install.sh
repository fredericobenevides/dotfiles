install_vim_plug() {
  echo "Installing the plugin vim-plug to manage the vim plugins"
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
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

install_matchit_plugin

install_vim_plug
launch_vim_to_install_all_plugins

