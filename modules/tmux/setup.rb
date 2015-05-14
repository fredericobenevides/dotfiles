def install_module
  install 'TMUX' do
    description 'Installing Tmux'

    when_os :mac do
      run 'brew install tmux'
    end

    when_os :linux do
      run 'sudo apt-get install tmux'

      description 'Added the TERM environment in ~/.zshrc'
      run %q(echo 'export TERM="screen-256color"' >> ~/.zshrc)
    end

    description 'Linking tmux configuration file'
    link from: 'tmux/tmux.conf', to: '~/', make_hidden: true
  end
end

def uninstall_module
  uninstall 'TMUX' do
    description 'Uninstalling Tmux'

    when_os :mac do
      run 'brew uninstall tmux'
    end

    when_os :linux do
      run 'sudo apt-get purge tmux'

      description 'Removing the TERM environment from ~/.zshrc'
      run 'sed -i "/TERM/d" ~/.zshrc'
    end

    description 'Unlinking tmux configuration file'
    unlink from: 'tmux/tmux.conf', to: '~/', make_hidden: true
  end
end
