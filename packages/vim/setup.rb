def setup_vim
  if OS.mac?
    system 'brew install macvim'
  else
    system 'sudo apt-get install vim-nox'
  end

  system 'git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim'

  copy_file from: 'vim/*.vim',  to: '~/.vim'
  copy_file from: 'vim/.vimrc', to: '~/'

  system 'vim +PluginInstall +qall'
end
