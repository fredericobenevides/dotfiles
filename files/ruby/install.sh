#!/bin/bash
set -e

homebrew=
type -p brew >/dev/null && homebrew=1

RUBY_VERSION=2.5.0

download_ruby() {
  curl -fsSL https://github.com/rbenv/rbenv-installer/raw/master/bin/rbenv-installer | bash
}

load_rbenv() {
  if [ ! -n "$homebrew" ]; then
    export RBENV_ROOT="${HOME}/.rbenv"
    export PATH="${RBENV_ROOT}/bin:${PATH}"
  fi

  eval "$(rbenv init -)"
}

install_ruby() {
  load_rbenv

  rbenv install $RUBY_VERSION
  rbenv global  $RUBY_VERSION
}

install_gems() {
  load_rbenv

  gem install awesome_print
  gem install bundler
  gem install hirb
  gem install interactive_editor
  gem install rails
  gem install rubocop
  gem install ruby_clone
  gem install tmuxinator
}

download_ruby
install_ruby
install_gems
