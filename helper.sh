#!/bin/bash

OS_MAC=`uname -sm | awk '{print $1}'`
OS_LINUX=""
if [[ -e "/etc/os-release" ]]; then
  OS_LINUX=`cat /etc/os-release | grep ^ID= | awk -F = '{print $2}'`
fi

exist_command() {
  command -v $1 &> /dev/null
}

is_macos() {
  if [ $OS_MAC == "Darwin" ]; then
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

is_manjaro() {
  if [[ $OS_LINUX == "manjaro" ]]; then
    return 0;
  else
    return 1;
  fi
}
