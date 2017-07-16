NODE_VERSION=6.11.1

download_node() {
  wget -q -O- https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-x64.tar.gz | tar xzvf - -C /tmp
  sudo mv /tmp/node-v$NODE_VERSION-linux-x64 /opt 2> /dev/null

  sudo unlink /opt/node 2> /dev/null
  sudo ln -s /opt/node-v$NODE_VERSION-linux-x64 /opt/node
}

set_node_on_path() {
  grep -q "/opt/node/bin" ~/.zshrc.local; if [ $? -ne 0 ];then echo "export PATH=\"/opt/node/bin:\$PATH\"" >> ~/.zshrc.local; fi
}

download_node
set_node_on_path
