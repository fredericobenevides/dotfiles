def install_module
  install 'NODE' do
    when_os :mac do
      description 'Installing Node'
      run 'brew install node'
    end

    when_os :linux do
      version = 'v4.2.3'

      description 'Installing Node'
      run "wget -q -O- https://nodejs.org/dist/#{version}/node-#{version}-linux-x64.tar.gz | tar xzvf - -C /tmp"

      description 'Moving the node folder to /opt'
      run "sudo mv /tmp/node-#{version}-linux-x64 /opt"

      description "Creating a link of node-#{version}-linux-x64.tar.gz to /opt/node"
      run "sudo ln -s /opt/node-#{version}-linux-x64 /opt/node"

      description 'Exporting nodejs path'
      run 'grep -q "/opt/node/bin" ~/.zshrc.local; if [ $? -ne 0 ];then echo "export PATH=\"/opt/node/bin:\$PATH\"" >> ~/.zshrc.local; fi'
    end
  end
end

def uninstall_module
  uninstall 'NODE' do
    when_os :mac do
      description 'Uninstalling Node'
      run 'brew uninstall node'
    end

    when_os :linux do
      version = 'v4.2.3'

      description 'Uninstalling Node'
      run "sudo rm -rf /opt/node-#{version}-linux-x64"

      description 'Unlink node in /opt/node'
      run 'sudo unlink /opt/node'

      description 'Removing nodejs path'
      run 'sed -i "/\/opt\/node/d" ~/.zshrc.local'
    end
  end
end
