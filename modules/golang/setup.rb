def install_module
  install 'GOLANG' do

    when_os :mac do
      description 'Installing golang'
      run 'brew install go'
    end

    when_os :linux do
      version = '1.5.3'

      description 'Installing golang'
      run "wget -q -O- https://storage.googleapis.com/golang/go#{version}.linux-amd64.tar.gz | tar xzvf - -C /tmp/"

      description 'Moving the go folder to /opt'
      run "sudo mv /tmp/go /opt"

      description 'Exporting go path'
      run 'grep -q "/opt/go/bin" ~/.zshrc.local; if [ $? -ne 0 ];then echo "export GOROOT=\"/opt/go\"" >> ~/.zshrc.local; echo "export PATH=\"/opt/go/bin:\$PATH\"" >> ~/.zshrc.local; fi'
    end
  end
end

def uninstall_module
  uninstall 'GOLANG' do
    description 'Uninstalling golang'

    when_os :mac do
      run 'brew uninstall go'
    end

    when_os :linux do
      version = '1.5.3'

      description 'Uninstalling golang'
      run 'sudo rm -rf /opt/go'

      description 'Removing go path'
      run 'sed -i "/\/opt\/go/d" ~/.zshrc.local'
    end
  end
end
