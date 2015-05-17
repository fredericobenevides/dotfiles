def install_module
  install 'ZSH' do
    description 'Installing zsh'
    run 'curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | sed "s/exit/exit -1/" | bash'

    add_default_editor

    change_zshr_to_use_theme_plugins

    create_zsh_folder
    change_zshr_to_load_files
  end
end

def uninstall_module
  uninstall 'ZSH' do
    description 'Removing the oh-my-zsh folder'
    run 'rm -rf ~/.oh-my-zsh'

    description 'Removing the .zshrc file'
    run 'rm ~/.zshrc'

    description 'Removing the .zsh folder'
    run 'rm -rf ~/.zsh'
  end
end

def add_default_editor
  command = %q(gsed -i "s/#   export EDITOR='mvim'/export EDITOR='vim'/g" ~/.zshrc)

  when_os :mac do
    run command
  end

  when_os :linux do
    run command.gsub('gsed', 'sed')
  end
end

def create_zsh_folder
    description 'Creating the .zsh folder and the subfolders'
    run 'mkdir -p ~/.zsh/aliases'
    run 'mkdir -p ~/.zsh/configs'
end

def change_zshr_to_load_files
    command = <<-EOF
grep "config_file" ${HOME}/.zshrc
if [ $? -ne 0 ]; then

  echo '
for config_file ($HOME/.zsh/**/*.zsh); do
  source $config_file
done' >> ${HOME}/.zshrc
fi
    EOF

    description 'Changing zshrc to load zsh files inside .zsh'
    run command
end

def change_zshr_to_use_theme_plugins
    description 'Linking theme'
    link from: 'zsh/themes/fredericobenevides.zsh-theme', to: '~/.oh-my-zsh/themes/fredericobenevides.zsh-theme'

    description 'Linking with the plugins folder'
    link from: 'zsh/plugins/*', to: '~/.oh-my-zsh/custom/plugins'

    description 'Changing zshrc to use the new theme and plugins'
    command_plugin = 'gsed -i "s/plugins=(git)/plugins=(brew bundler gem git fredericobenevides)/g" ~/.zshrc'
    command_theme  = 'gsed -i "s/robbyrussell/fredericobenevides/g" ~/.zshrc'

    when_os :mac do
      run command_plugin
      run command_theme
    end

    when_os :linux do
      run command_plugin.gsub('gsed', 'sed')
      run command_theme.gsub('gsed', 'sed')
    end
end
