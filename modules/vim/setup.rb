def install_vim
  install 'VIM' do
    when_os OS.mac? do
      description 'Installing MacVim'
      run 'brew install macvim'

      description 'Adding alias to mvim in .zshrc'
      run %q(echo 'alias vim="mvim"' >> ~/.zshrc)

      description 'Linking MacVim to Applications'
      run 'brew linkapps macvim'
    end

    when_os OS.linux? do
      description 'Installing vim'
      run 'sudo apt-get install gvim'

      description 'Adding alias to gvim in .zshrc'
      run %q(echo 'alias vim="gvim"' >> ~/.zshrc)
    end

    description 'Installing the plugin Vundle to manage the vim plugins'
    run 'git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim'

    description 'Linking vimrc file'
    link from: 'vim/vimrc', to: '~/', make_hidden: true

    description 'Linking .vim files'
    link from: 'vim/*.vim', to: '~/.vim/'

    description 'Launching vim to install the plugins'
    run 'vim +PluginInstall +qall'

    description 'Installing cmake to use with the plugin YouCompleteMe'
    run 'brew install cmake'

    description 'Installing the plugin YouCompleteMe'
    run '~/.vim/bundle/YouCompleteMe/install.sh'
  end
end

def uninstall_vim
  uninstall 'VIM' do
    description 'Removing alias of vim in .zshrc'
    delete_alias = 'gsed -i "/vim=/d" ~/.zshrc'

    when_os OS.mac? do
      run delete_alias

      description 'Unlinking MacVim to Applications'
      run 'brew unlinkapps macvim'

      description 'Uninstalling MacVim'
      run 'brew uninstall macvim'
    end

    when_os OS.linux? do
      run delete_alias.gsub('gsed', 'sed')

      description 'Uninstalling vim'
      run 'sudo apt-get purge vim'
    end

    description 'Unlinking vimrc file'
    unlink from: 'vim/vimrc', to: '~/', make_hidden: true

    description 'Removing the .vim folder'
    run 'rm -rf ~/.vim'

    description 'Uninstalling cmake'
    run 'brew uninstall cmake'
  end
end
