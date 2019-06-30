#!/bin/bash

OS=`uname -sm | awk '{print $1}'`
OS_LINUX=`cat /etc/os-release | grep ^ID | awk -F = '{print $2}'`

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

is_debian() {
  if [[ $OS_LINUX == "debian" ]]; then
    return 0;
  else
    return 1;
  fi
}

is_ubuntu() {
  if [[ $OS_LINUX == "ubuntu" ]]; then
    return 0;
  else
    return 1;
  fi
}
