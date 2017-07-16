GO_VERSION=1.8.3

download_go() {
  wget -q -O- https://storage.googleapis.com/golang/go$GO_VERSION.linux-amd64.tar.gz | tar xzvf - -C /tmp/
  sudo mv /tmp/go /opt/go$GO_VERSION 2> /dev/null

  sudo unlink /opt/go 2> /dev/null
  sudo ln -s /opt/go$GO_VERSION /opt/go
}

set_go_on_path() {
  grep -q "/opt/go/bin" ~/.zshrc.local; if [ $? -ne 0 ];then echo "export GOROOT=\"/opt/go\"" >> ~/.zshrc.local; echo "export PATH=\"/opt/go/bin:\$PATH\"" >> ~/.zshrc.local; fi
}

download_go
set_go_on_path
