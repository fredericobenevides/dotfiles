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
. ./modules/docker.sh
. ./modules/emacs.sh
. ./modules/flutter.sh
. ./modules/fzf.sh
. ./modules/git.sh
. ./modules/i3.sh
. ./modules/idea.sh
. ./modules/nerd-fonts.sh
. ./modules/nodejs.sh
. ./modules/sdkman.sh
. ./modules/xorg.sh
. ./modules/zsh.sh




###### EMACS
#install_title "emacs"

#pacman "emacs"

#echo -e "Configuring emacs"

#echo -e "Creating the folder ~/.emacs.d"
#mkdir -p ~/.emacs.d

#ln -s ~/.dotfiles/files/emacs/init.el ~/.emacs.d/init.el
#ln -s ~/.dotfiles/files/emacs/init.org ~/.emacs.d/init.org

#finished_title "emacs"

#curl -o /tmp/idea https://download.jetbrains.com/idea/ideaIU-2023.1.7.tar.gz
#sudo mkdir -p /opt/idea

#sudo tar -C /opt/idea --strip-components=1 -xzvf /tmp/idea

###### EMACS

