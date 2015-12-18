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
    run 'git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim'

    description 'Linking vimrc file'
    link from: 'vim/files/vimrc', to: '~/', make_hidden: true

    description 'Linking .vim files'
    link from: 'vim/files/*.vim', to: '~/.vim/'

    description 'Launching vim to install the plugins'
    run 'vim +PluginInstall +qall'

    description 'Installing the plugin YouCompleteMe'
    run '~/.vim/bundle/YouCompleteMe/install.py --gocode-completer'
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
