def install_module
  install 'TMUX' do
    description 'Installing Tmux'

    when_os :mac do
      run 'brew install tmux'
    end

    when_os :linux do
      run 'sudo apt-get install tmux'
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
    end

    description 'Unlinking tmux configuration file'
    unlink from: 'tmux/tmux.conf', to: '~/', make_hidden: true
  end
end
