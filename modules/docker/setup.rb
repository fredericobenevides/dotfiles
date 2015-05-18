def install_module
  install 'DOCKER' do
    when_os :mac do
      description 'Installing  VirtualBox'
      run 'brew cask install virtualbox'

      description 'Installing docker'
      run 'brew install docker'
      run 'brew install boot2docker'

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

      remove_boot2docker_folder
    end

    when_os :linux do
      description 'Uninstalling docker'
      run 'sudo apt-get purge docker'
    end
  end
end

def remove_boot2docker_folder
  run 'rm -rf ~/"VirtualBox VMs"/boot2docker-vm'
end
