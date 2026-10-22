DOTS_DIR := $(HOME)/.dotfiles/dots

include makefiles/clojure.mk
include makefiles/cloudflare.mk
include makefiles/cursor.mk
include makefiles/docker.mk
include makefiles/emacs.mk
include makefiles/flatpak.mk
include makefiles/flutter.mk
include makefiles/fzf.mk
include makefiles/gcloud.mk
include makefiles/git.mk
include makefiles/go.mk
include makefiles/hyprland.mk
include makefiles/idea.mk
include makefiles/k8s.mk
include makefiles/keyboard.mk
include makefiles/kitty.mk
include makefiles/neovim.mk
include makefiles/fonts.mk
include makefiles/nodejs.mk
include makefiles/pacman.mk
include makefiles/python.mk
include makefiles/quickshell.mk
include makefiles/rust.mk
include makefiles/sdkman.mk
include makefiles/swap.mk
include makefiles/vscode.mk
include makefiles/zsh.mk

.DEFAULT_GOAL := all

system: swap-all pacman-all cloudflare-all

gui: keyboard-all fonts-all hyprland-all quickshell-all cursor-all kitty-all \
     pacman-gui-all flatpak-gui-all

tools: fzf-all git-all zsh-all

dev: sdkman-all clojure-all flutter-all go-all nodejs-all python-all rust-all \
     docker-all k8s-all gcloud-all pacman-dev-all flatpak-dev-all

editors: emacs-all idea-all neovim-all vscode-all

all: system gui tools dev editors
