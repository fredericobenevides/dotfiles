#!/usr/bin/env bash

if [ -d ~/.cargo ]; then
  info "rust already configured!"
  return
fi

info "Installing and configuration rust"

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

rustup component add rust-analyzer

info "Finished installing and configuring rust"
