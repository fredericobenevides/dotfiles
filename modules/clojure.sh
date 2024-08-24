#!/usr/bin/env bash

/usr/bin/pacman -Qi clojure > /dev/null 2>&1
if [ $? -eq 0 ]; then
  info "clojure already configured!"
  return
fi

info "Installing and configuring clojure"

pacman "clojure"
pacman "leiningen"

info "Finished installing and configuring clojure"