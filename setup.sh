#!/bin/bash

source ./helper.sh

OS="linux"
OS_DISTRO=""

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
      case "$OS_DISTRO" in
        "ubuntu")
          sudo add-apt-repository --yes --update ppa:git-core/ppa
          sudo apt-get update
          sudo apt-get install -y git
          ;;
        "debian")
          sudo apt-get install -y git
          ;;
        *)
          echo "I don't know how to install git for $OS_LINUX"
          exit 1
      esac
    fi
  fi

}

install_ansible() {
  if ! exist_command ansible-playbook; then
    echo "### Installing ansible"

    if is_macos; then
      brew install ansible
    else
      case "$OS_DISTRO" in
        "ubuntu")
          sudo add-apt-repository --yes --update ppa:ansible/ansible
          sudo apt-get update
          sudo apt-get install -y ansible
          ;;
        "debian")
          value=`grep ansible/ubuntu /etc/apt/sources.list`
          if [[ -z "$VALUE" ]]; then
            echo "deb http://ppa.launchpad.net/ansible/ansible/ubuntu trusty main" | sudo tee -a /etc/apt/sources.list
            sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 93C4A3FD7BB9C367 A1715D88E1DF1F24
            sudo apt-get update
            sudo apt-get install -y ansible
          fi
          ;;
        *)
          echo "I don't know how to install ansible for $OS_LINUX"
          exit 1
      esac
    fi
  fi
}

run_ansible() {
  ansible-playbook --ask-become-pass -i ansible/hosts ansible/setup.yaml \
    --extra-vars "username=`id -u`" \
    --extra-vars "usergroup=`id -g`" \
    --extra-vars "dotfilespath=`pwd`" \
    --extra-vars "os=$OS" \
    --extra-vars "os_distro=$OS_DISTRO" \
    $1
}

if is_macos; then
  install_homebrew
  OS="macos"
elif is_debian; then
  set_env_codename_debian
  OS_DISTRO="debian"
else
  set_env_codename_ubuntu
  OS_DISTRO="ubuntu"
fi

install_git
install_ansible

run_ansible $1
