#!/usr/bin/env bash

# Temporaty while I'm having a problem to use docker from manjaro

uninstall() {
  for i in `ls /opt/docker`; do
    sudo unlink /usr/local/bin/$i
  done

  sudo rm -rf /opt/docker
}

install() {
  uninstall

  curl -Lo /tmp/docker.tgz https://download.docker.com/linux/static/stable/x86_64/docker-19.03.14.tgz

  tar -xzvf /tmp/docker.tgz -C /tmp

  sudo mv /tmp/docker /opt

  for i in `ls /opt/docker`; do
    sudo ln -s /opt/docker/$i /usr/local/bin/$i
  done
}

uninstall_docker_compose() {
  sudo unlink /usr/local/bin/docker-compose

  sudo rm /opt/docker-compose
}

install_docker_compose() {
  curl -Lo /tmp/docker-compose "https://github.com/docker/compose/releases/download/1.27.4/docker-compose-$(uname -s)-$(uname -m)"

  sudo mv /tmp/docker-compose /opt

  sudo ln -s /opt/docker-compose /usr/local/bin/docker-compose

  sudo chmod +x /opt/docker-compose
}

$*
