def install_module
  install 'FZF - Fuzzy Finder'do
    description 'Installing the fzf'

    description 'Cloning its repository'
    run 'git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf 2> /dev/null'
  end
end

def uninstall_module
  uninstall 'FZF - Fuzzy Finder'do
    description 'Uninstalling the fzf'

    description 'Removing the directory'
    run 'rm -rf ~/.fzf'
  end

end
