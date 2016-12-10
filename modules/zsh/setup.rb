def install_module
  install 'ZSH' do
    description 'Installing zsh'
    run 'git clone --depth=1 https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh'

    description 'Linking zshrc file'
    link from: 'zsh/files/zshrc', to: '~/', make_hidden: true

    description 'Linking theme'
    link from: 'zsh/themes/fredericobenevides.zsh-theme', to: '~/.oh-my-zsh/themes/fredericobenevides.zsh-theme'

    description 'Linking with the plugins folder'
    link from: 'zsh/plugins/*', to: '~/.oh-my-zsh/custom/plugins'

    description 'Changing current shell to zsh'
    run 'chsh -u frederico -s /bin/zsh'
  end
end

def uninstall_module
  uninstall 'ZSH' do
    description 'Removing the oh-my-zsh folder'
    run 'rm -rf ~/.oh-my-zsh'

    description 'Removing the .zshrc file'
    run 'rm ~/.zshrc'
  end
end
