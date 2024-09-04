#!/usr/bin/env bash

function install_nodejs() {
    if [ -d ~/.fnm ]; then
        info "nodejs already configured!"
        return
    fi

    info "Installing and configuring nodejs"

    echo -e "-- Installing fnm to manage nodejs"
    curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir $HOME/.fnm --skip-shell

    echo -e "-- Preparing fnm environment"
    export PATH=$HOME/.fnm:$PATH
    eval "$(fnm env --use-on-cd --version-file-strategy=recursive)"

    echo -e "-- Installing nodejs"
    fnm install v20.17.0

    echo -e "-- Setting default nodejs"
    fnm default v20.17.0

    info "Finished installing and configuring nodejs"
}

function install_pkgs_nodejs() {
  info "Installing nodejs packages"

  npm "prettier"
  npm "typescript"
  npm "typescript-language-server"
  npm "vscode-langservers-extracted"

  info "Finished installing nodejs packages"
}

install_nodejs
install_pkgs_nodejs
