def install_module
  install 'GIT' do
    description 'Installing git'

    when_os :mac do
      run 'brew install git'
    end

    when_os :linux do
      run 'sudo apt-get install git-core'
    end

    description 'Linking git files'
    link from: 'git/git*', to: '~/', make_hidden: true
  end
end

def uninstall_module
  uninstall 'GIT' do
    description 'Uninstalling git'

    when_os :mac do
      run 'brew uninstall git'
    end

    when_os :linux do
      run 'sudo apt-get purge git-core'
    end

    description 'Unlinking git files'
    unlink from: 'git/git*', to: '~/', make_hidden: true
  end
end
