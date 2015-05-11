def install_git
  install 'GIT' do
    description 'Installing git'

    when_os OS.mac? do
      run 'brew install git'
    end

    when_os OS.linux? do
      run 'sudo apt-get install git-core'
    end

    description 'Linking git files'
    link from: 'git/git*', to: '~/', make_hidden: true
  end
end

def uninstall_git
  uninstall 'GIT' do
    description 'Uninstalling git'

    when_os OS.mac? do
      run 'brew uninstall git'
    end

    when_os OS.linux? do
      run 'sudo apt-get purge git-core'
    end

    description 'Unlinking git files'
    unlink from: 'git/git*', to: '~/', make_hidden: true
  end
end
