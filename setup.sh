#!/usr/bin/env bash

clear

. ./modules/utils.sh

# system package
. ./modules/swap.sh

# general packages
. ./modules/pacman.sh
. ./modules/flatpak.sh

# packages
. ./modules/clojure.sh
. ./modules/desktop.sh
. ./modules/docker.sh
. ./modules/emacs.sh
. ./modules/flutter.sh
. ./modules/fzf.sh
. ./modules/git.sh
. ./modules/hyprland.sh
. ./modules/idea.sh
. ./modules/nerd-fonts.sh
. ./modules/nodejs.sh
. ./modules/python.sh
. ./modules/rofi.sh
. ./modules/rust.sh
. ./modules/sdkman.sh
. ./modules/vscode.sh
. ./modules/waybar.sh 
. ./modules/zsh.sh
