#!/bin/bash

OS=`uname -sm | awk '{print $1}'`

exist_command() {
  command -v $1 &> /dev/null
}

is_macos() {
  return "$OS" = "Darwin"
}

install_homebrew() {
  if ! exist_command brew; then
    echo "### Installing homebrew"
    ( exec /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" )
    username=`whoami`
    `sudo chown -R ${username} /usr/local`

    brew install git
  fi
}

install_ansible() {
  if ! exist_command ansible-playbook; then
    echo "### Installing ansible"

    if [ is_macos ]; then
      brew install ansible
    else
      sudo apt-get install ansible
    fi
  fi
}

if [ is_macos ]; then
  install_homebrew
  brew install git
else
  sudo apt-get install git-core
fi

install_ansible

./setup.sh
