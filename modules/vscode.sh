#!/usr/bin/env bash

function install_vscode() {
    /usr/bin/pacman -Qi code > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        info "vscode already configured!"
        return
    fi

    info "Installing and configuring vscode"

    yay "visual-studio-code-bin"

    info "Finished installing and configuring vscode"    
}

function install_vscode_extensions() {
    info "Installing VSCode extensions"

    # Themes & Icons
    code "Catppuccin.catppuccin-vsc"
    code "vscode-icons-team.vscode-icons"

    # Editor Enhancements
    code "alefragnani.Bookmarks"
    code "bierner.emojisense"
    code "ritwickdey.LiveServer"
    code "usernamehw.find-jump"
    code "vadimcn.vscode-lldb"
    code "wayou.vscode-todo-highlight"

    # Colors & Visuals
    code "oderwat.indent-rainbow"
    code "kisstkondoros.vscode-gutter-preview"
    code "mikestead.dotenv"

    # Git
    code "donjayamanne.githistory"
    code "eamodio.gitlens"

    # Docker
    code "ms-azuretools.vscode-docker"

    # Go
    code "golang.Go"

    # Rust
    code "rust-lang.rust-analyzer"
    code "tamasfe.even-better-toml"

    # Formatting & Code Quality
    code "esbenp.prettier-vscode"
    code "fill-labs.dependi"

    # HTML Tools
    code "formulahendry.auto-rename-tag"

    # JavaScript
    code "dbaeumer.vscode-eslint"
    code "xabikos.JavaScriptSnippets"

    # CSS
    code "bradlc.vscode-tailwindcss"
    code "kamikillerto.vscode-colorize"
    code "pranaygp.vscode-css-peek"

    info "Finished installing VSCode extensions"
}

link_files() {
    info "-- Linking files"

    ln -sf ~/.dotfiles/files/vscode/settings.json ~/.config/Code/User/settings.json
    ln -sf ~/.dotfiles/files/vscode/keybindings.json ~/.config/Code/User/keybindings.json
}

install_vscode
install_vscode_extensions
link_files
