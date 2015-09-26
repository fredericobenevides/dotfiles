def install_module
  install 'DOCKER' do
    when_os :mac do
      description 'Installing  VirtualBox'
      run 'brew cask install virtualbox'

      description 'Installing docker'
      run 'brew install docker'

      description 'Installing docker machine'
      run 'brew cask install docker-machine'

      description 'Init boot2docker'
      run '/usr/local/bin/boot2docker init'
    end

    when_os :linux do
      description 'Installing docker'
      run 'sudo apt-get install docker'
    end
  end
end

def uninstall_module
  uninstall 'DOCKER' do
    when_os :mac do
      description 'Uninstalling  VirtualBox'
      run 'brew cask uninstall virtualbox'

      description 'Uninstalling docker'
      run 'brew uninstall docker'
      run 'brew uninstall boot2docker'

      description 'Uninstalling docker machine'
      run 'brew cask uninstall docker-machine'

      description 'Removing boot2docker-vm folder'
      run 'rm -rf ~/"VirtualBox VMs"/boot2docker-vm'
    end

    when_os :linux do
      description 'Uninstalling docker'
      run 'sudo apt-get purge docker'
    end
  end
end
