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

    # editor
    code "alefragnani.Bookmarks"
    code "ritwickdey.LiveServer"
    code "tuttieee.emacs-mcx"
    code "usernamehw.find-jump"
    code "tamasfe.even-better-toml"    
    code "vadimcn.vscode-lldb"
    code "wayou.vscode-todo-highlight"

    # colors
    code "kamikillerto.vscode-colorize"
    code "oderwat.indent-rainbow"
    code "vscode-icons-team.vscode-icons"

    # docker
    code "ms-azuretools.vscode-docker"

    # git
    code "donjayamanne.githistory"    
    code "eamodio.gitlens"

    # html, css, javascript, angular
    code "angular.ng-template"
    code "gydunhn.angular-essentials"
    code "xabikos.JavaScriptSnippets"
    code "formulahendry.auto-rename-tag"
    code "pranaygp.vscode-css-peek"
    code "bradlc.vscode-tailwindcss"
    code "dbaeumer.vscode-eslint"
    code "esbenp.prettier-vscode"

    # rust
    code "serayuzgur.crates"
    code "rust-lang.rust-analyzer"

    info "Finished installing vscode extensions"
}

install_vscode
install_vscode_extensions
