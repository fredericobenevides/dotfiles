def install_module
  install 'TMUX' do
    description 'Installing Tmux'

    when_os :mac do
      run 'brew install tmux'
      run 'brew install reattach-to-user-namespace'
    end

    when_os :linux do
      run 'sudo apt-get install tmux'
      run 'sudo apt-get install xclip'

      description 'Linking the tmux.zsh to .zsh configs folder'
      link from: 'tmux/tmux.zsh', to: '~/.zsh/configs'
    end

    description 'Linking tmux configuration file'
    link from: 'tmux/tmux.conf', to: '~/', make_hidden: true

    description 'Linking tmux_clipboard.sh to /usr/local/bin/tm'
    link from: 'tmux/tmux_clipboard.sh', to: '/usr/local/bin/'

    description 'Linking tmux_start.sh to /usr/local/bin/tm'
    link from: 'tmux/tmux_start.sh', to: '/usr/local/bin/tm'
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

      description 'Unlinking the tmux.zsh to .zsh configs folder'
      link from: 'tmux/tmux.zsh', to: '~/.zsh/configs'
    end

    description 'Unlinking tmux configuration file'
    unlink from: 'tmux/tmux.conf', to: '~/', make_hidden: true

    description 'Unlinking tmux_start.sh to /usr/local/bin/tm'
    unlink from: 'tmux/tmux_start.sh', to: '/usr/local/bin/tm'
  end
end
