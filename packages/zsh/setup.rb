def setup_zsh
  system 'curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | bash'

  copy_file   :from => 'zsh/themes/fredericobenevides.zsh-theme', :to => '~/.oh-my-zsh/themes/fredericobenevides.zsh-theme'
  copy_folder :from => 'zsh/plugins', :to => '~/.oh-my-zsh/custom'

  puts '==> Updating the .zshrc to have the new plugins and theme'

  command_plugin = 'gsed -i "s/plugins=(git)/plugins=(brew bundler gem git fredericobenevides)/g" ~/.zshrc'
  command_theme  = 'gsed -i "s/robbyrussell/fredericobenevides/g" ~/.zshrc'

  if OS.mac?
    system 'brew install gnu-sed'

    system command_plugin
    system command_theme
  else
    system command_plugin.gsub('gsed', 'sed')
    system command_theme.gsub('gsed', 'sed')
  end
end
