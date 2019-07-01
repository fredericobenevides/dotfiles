#!/usr/bin/env bash

set -u

git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf

[[ "$@" =~ --pre ]] && pre=1 || pre=0
version=`grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+' ~/.fzf/install  | head -1`

binary_arch=
fzf_base="$HOME/.fzf"

ask() {
  # If stdin is a tty, we are "interactive".
  # non-interactive shell: wait for a linefeed
  #     interactive shell: continue after a single keypress
  read_n=$([ -t 0 ] && echo "-n 1")

  read -p "$1 ([y]/n) " $read_n -r
  echo
  [[ $REPLY =~ ^[Nn]$ ]]
}

check_binary() {
  echo -n "  - Checking fzf executable ... "
  local output
  output=$("$fzf_base"/bin/fzf --version 2>&1)
  if [ $? -ne 0 ]; then
    echo "Error: $output"
    binary_error="Invalid binary"
  elif [ "$version" != "$output" ]; then
    echo "$output != $version"
    binary_error="Invalid version"
  else
    echo "$output"
    binary_error=""
    return 0
  fi
  rm -f "$fzf_base"/bin/fzf
  return 1
}

symlink() {
  echo "  - Creating symlink: bin/$1 -> bin/fzf"
  (cd "$fzf_base"/bin &&
   rm -f fzf &&
   ln -sf $1 fzf)
  if [ $? -ne 0 ]; then
    binary_error="Failed to create symlink"
    return 1
  fi
}

link_fzf_in_path() {
  if which_fzf="$(command -v fzf)"; then
    echo "  - Found in \$PATH"
    echo "  - Creating symlink: $which_fzf -> bin/fzf"
    (cd "$fzf_base"/bin && rm -f fzf && ln -sf "$which_fzf" fzf)
    check_binary && return
  fi
  return 1
}

try_curl() {
  command -v curl > /dev/null && curl -fL $1 | tar -xz
}

try_wget() {
  command -v wget > /dev/null && wget -O - $1 | tar -xz
}

download() {
  echo "Downloading bin/fzf ..."
  if [ $pre = 0 ]; then
    if [ -x "$fzf_base"/bin/fzf ]; then
      echo "  - Already exists"
      check_binary && return
    fi
    if [ -x "$fzf_base"/bin/$1 ]; then
      symlink $1 && check_binary && return
    fi
    link_fzf_in_path && return
  fi
  mkdir -p "$fzf_base"/bin && cd "$fzf_base"/bin
  if [ $? -ne 0 ]; then
    binary_error="Failed to create bin directory"
    return
  fi

  local url=https://github.com/junegunn/fzf-bin/releases/download/$version/${1}.tgz
  set -o pipefail
  if ! (try_curl $url || try_wget $url); then
    set +o pipefail
    binary_error="Failed to download with curl and wget"
    return
  fi
  set +o pipefail

  if [ ! -f $1 ]; then
    binary_error="Failed to download ${1}"
    return
  fi

  chmod +x $1 && symlink $1 && check_binary
}

# Try to download binary executable
archi=$(uname -sm)
binary_available=1
binary_error=""
case "$archi" in
  Darwin\ x86_64) download fzf-$version-darwin_${binary_arch:-amd64} ;;
  Darwin\ i*86)   download fzf-$version-darwin_${binary_arch:-386}   ;;
  Linux\ x86_64)  download fzf-$version-linux_${binary_arch:-amd64}  ;;
  Linux\ i*86)    download fzf-$version-linux_${binary_arch:-386}    ;;
  *)              binary_available=0 binary_error=1  ;;
esac

echo 'For more information, see: https://github.com/junegunn/fzf'

