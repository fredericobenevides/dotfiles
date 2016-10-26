def install_module
  install 'VIM' do
    when_os :mac do
      description 'Installing MacVim'
      run 'brew install macvim --override-system-vim'

      description 'Linking MacVim to Applications'
      run 'brew linkapps macvim'

      description 'Installing cmake to use with the plugin YouCompleteMe'
      run 'brew install cmake'

      description 'Installing Exuberant Ctags'
      run 'brew install ctags-exuberant'
    end

    when_os :linux do
      description 'Installing vim-gtk'
      run 'sudo apt-get install vim-gtk'

      description 'Installing Exuberant Ctags'
      run 'sudo apt-get install exuberant-ctags'
    end

    description 'Installing the plugin Vundle to manage the vim plugins'
    run 'curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

    description 'Linking vimrc file'
    link from: 'vim/files/vimrc', to: '~/', make_hidden: true

    description 'Linking .vim files'
    link from: 'vim/files/*.vim', to: '~/.vim/'

    description 'Launching vim to install the plugins'
    run 'vim +PlugInstall +qall'

    description 'Installing the plugin tern_for_vim'
    run 'echo "npm --prefix ~/.vim/bundle/tern_for_vim install ~/.vim/bundle/tern_for_vim" > ~/.vim/bundle/tern_for_vim/install.sh'
    run 'sh ~/.vim/bundle/tern_for_vim/install.sh'

    description 'Copying matchit plugin from $VIMRUNTIME'
    run 'mkdir -p ~/.vim/plugin'
    run %q(vim -e --cmd 'exe "set t_cm=\<C-M>"|!cp $VIMRUNTIME/macros/matchit.vim ~/.vim/plugin' +visual +qall)

    description 'Copying matchit doc plugin from $VIMRUNTIME'
    run 'mkdir -p ~/.vim/doc'
    run %q(vim -e --cmd 'exe "set t_cm=\<C-M>"|!cp $VIMRUNTIME/macros/matchit.txt ~/.vim/doc' +visual +qall)
    run %q(vim -e --cmd 'exe "set t_cm=\<C-M>"|helptags ~/.vim/doc' +visual +qall)
  end
end

def uninstall_module
  uninstall 'VIM' do
    when_os :mac do
      description 'Unlinking MacVim to Applications'
      run 'brew unlinkapps macvim'

      description 'Uninstalling MacVim'
      run 'brew uninstall macvim'

      description 'Uninstalling cmake'
      run 'brew uninstall cmake'

      description 'Uninstalling Exuberant Ctags'
      run 'brew uninstall ctags-exuberant'
    end

    when_os :linux do
      description 'Uninstalling vim-gtk'
      run 'sudo apt-get purge vim-gtk'

      description 'Uninstalling Exuberant Ctags'
      run 'sudo apt-get purge exuberant-ctags'
    end

    description 'Unlinking vimrc file'
    unlink from: 'vim/files/vimrc', to: '~/', make_hidden: true

    description 'Removing the .vim folder'
    run 'rm -rf ~/.vim'
  end
end
