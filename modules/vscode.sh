#!/usr/bin/env bash

function install_vscode() {
    /usr/bin/pacman -Qi code > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        info "vscode already configured!"
        return
    fi

    info "Installing and configuring vscode"

    pacman "code"

    info "Finished installing and configuring vscode"    
}

function install_vscode_extensions() {
    info "Installing vscode extensions"

    code "usernamehw.find-jump"
    
    code "angular.ng-template"
    code "gydunhn.angular-essentials"

    code "esbenp.prettier-vscode"

    code "rust-lang.rust-analyzer"

    info "Finished installing vscode extensions"
}

install_vscode
install_vscode_extensions
