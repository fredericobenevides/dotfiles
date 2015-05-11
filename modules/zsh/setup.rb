def install_zsh
  install 'ZSH' do
    description 'Installing zsh'
    run 'curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | sed "s/exit/exit -1/" | bash'

    description 'Linking theme'
    link from: 'zsh/themes/fredericobenevides.zsh-theme', to: '~/.oh-my-zsh/themes/fredericobenevides.zsh-theme'

    description 'Linking with the plugins folder'
    link from: 'zsh/plugins/*', to: '~/.oh-my-zsh/custom/plugins'

    command_plugin = 'gsed -i "s/plugins=(git)/plugins=(brew bundler gem git fredericobenevides)/g" ~/.zshrc'
    command_theme  = 'gsed -i "s/robbyrussell/fredericobenevides/g" ~/.zshrc'

    description 'Changing zsh to use the new theme and plugins'

    when_os :mac do
      run command_plugin
      run command_theme
    end

    when_os :linux do
      run command_plugin.gsub('gsed', 'sed')
      run command_theme.gsub('gsed', 'sed')
    end

  end
end

def uninstall_zsh
  uninstall 'ZSH' do
    description 'Removing the oh-my-zsh folder'
    run 'rm -rf ~/.oh-my-zsh'

    description 'Removing the .zshrc file'
    run 'rm ~/.zshrc'
  end
end
