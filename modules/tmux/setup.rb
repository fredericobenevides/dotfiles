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
    end

    description 'Linking tmux configuration file'
    link from: 'tmux/files/tmux.conf', to: '~/', make_hidden: true

    description 'Linking tmux_clipboard.sh to /usr/local/bin/tm'
    link from: 'tmux/files/tmux_clipboard.sh', to: '/usr/local/bin/'

    description 'Linking tmux_start.sh to /usr/local/bin/tm'
    link from: 'tmux/files/tmux_start.sh', to: '/usr/local/bin/tm'
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
    end

    description 'Unlinking tmux configuration file'
    unlink from: 'tmux/files/tmux.conf', to: '~/', make_hidden: true

    description 'Unlinking tmux_start.sh to /usr/local/bin/tm'
    unlink from: 'tmux/files/tmux_start.sh', to: '/usr/local/bin/tm'
  end
end
