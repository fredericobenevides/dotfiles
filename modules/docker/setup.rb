def install_module
  install 'DOCKER' do
    when_os :mac do
      description 'Installing dockertoolbox'
      run 'brew cask install docker-toolbox'
    end

    when_os :linux do
      description 'Installing docker'
      run 'wget -qO- https://get.docker.com/ | sh'
    end
  end
end

def uninstall_module
  uninstall 'DOCKER' do
    when_os :mac do
      description 'Installing dockertoolbox'
      run 'brew cask uninstall docker-toolbox'

      description 'Removing .docker folder'
      run 'rm -rf ~/.docker'
    end

    when_os :linux do
      description 'Uninstalling docker'
      run 'sudo apt-get purge docker-engine'
    end
  end
end
