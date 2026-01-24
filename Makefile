include makefiles/clojure.mk
include makefiles/docker.mk
include makefiles/emacs.mk
include makefiles/flatpak.mk
include makefiles/flutter.mk
include makefiles/fzf.mk
include makefiles/git.mk
include makefiles/go.mk
include makefiles/hyperland.mk
include makefiles/idea.mk
include makefiles/keyboard.mk
include makefiles/neovim.mk
include makefiles/nerd-fonts.mk
include makefiles/nodejs.mk
include makefiles/pacman.mk
include makefiles/python.mk
include makefiles/rofi.mk
include makefiles/rust.mk
include makefiles/sdkman.mk
include makefiles/swap.mk
include makefiles/vscode.mk
include makefiles/waybar.mk
include makefiles/zsh.mk

.DEFAULT_GOAL := all

system: swap-all pacman-all flatpak-all docker-all

gui: keyboard-all nerd-fonts-all hyprland-all rofi-all waybar-all

langs: sdkman-all clojure-all flutter-all go-all nodejs-all python-all rust-all

tools: fzf-all git-all zsh-all

editors: emacs-all idea-all neovim-all vscode-all

all: system gui langs tools editors