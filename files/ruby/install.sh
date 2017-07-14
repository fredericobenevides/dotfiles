RUBY_VERSION=2.4.1

download_ruby() {
  curl https://raw.githubusercontent.com/fesplugas/rbenv-installer/master/bin/rbenv-installer | bash
}

load_rbenv() {
  <<-EOF
  export RBENV_ROOT="${HOME}/.rbenv"
  export PATH="${RBENV_ROOT}/bin:${PATH}"
  eval "$(rbenv init -)"
EOF
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
