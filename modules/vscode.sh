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

    # themes
    code "Catppuccin.catppuccin-vsc"
    code "PKief.material-icon-theme"

    # editor
    code "alefragnani.Bookmarks"
    code "bierner.emojisense"
    code "ritwickdey.LiveServer"
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

    # vuejs
    code "johnsoncodehk.volar"
    code "johnsoncodehk.vscode-typescript-vue-plugin"

    # rust
    code "serayuzgur.crates"
    code "rust-lang.rust-analyzer"

    info "Finished installing vscode extensions"
}

link_files() {
    if [[ -d ~/.config/code-flags.conf ]]; then
        info "code-flags already configured!"
        return
    fi

    info "-- Linking files"
    ln -sf ~/.dotfiles/files/vscode/code-flags.conf ~/.config/code-flags.conf
}

install_vscode
install_vscode_extensions
link_files

