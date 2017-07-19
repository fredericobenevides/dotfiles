#!/bin/bash

OS=`uname -sm | awk '{print $1}'`

exist_command() {
  command -v $1 &> /dev/null
}

is_macos() {
  if [ $OS == "Darwin" ]; then
    return 0;
  else
    return 1;
  fi
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

install_git() {
  if ! exist_command git; then
    echo "### Installing git"

    if is_macos; then
      brew install git
    else
      sudo apt-get install git
    fi
  fi

}

install_ansible() {
  if ! exist_command ansible-playbook; then
    echo "### Installing ansible"

    if is_macos; then
      brew install ansible
    else
      sudo apt-get install ansible
    fi
  fi
}

run_ansible() {
  ansible-playbook --ask-become-pass -i ansible/hosts ansible/setup.yaml --extra-vars "dotfilespath=`pwd`"
}

run_zsh() {
  env zsh
  source ~/.zshrc
}

if is_macos; then
  install_homebrew
fi

install_git
install_ansible

run_ansible
run_zsh
