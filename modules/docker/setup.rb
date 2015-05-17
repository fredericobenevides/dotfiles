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
    end

    remove_virtualbox_folder
  end
end

def remove_virtualbox_folder
  command = <<-EOF
    output=`ls ~/"VirtualBox VMs" | wc -l`
    if [ $output -eq "1" ]; then
      boot2docker=`ls ~/"VirtualBox VMs/" | grep boot2docker-vm | wc -l`
      if [ $boot2docker -eq "1" ]; then
        rm -rf ~/"VirtualBox VMs"
      else
        echo "Can't remove all the images, since the installed image is not the boot2docker"
        exit -1
      fi
    else
      echo "Can't remove the VirtualBox folder since has more than 1 image"
      exit -1
    fi
  EOF

  run command
end
