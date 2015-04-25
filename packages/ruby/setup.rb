def setup_ruby
  copy_file :from => 'ruby/.gemrc', :to => '~/'
  copy_file :from => 'ruby/.irbrc', :to => '~/'

  setup_rbenv
  install_ruby
  install_gems
end

def init_rbenv
  <<-EOF
    export RBENV_ROOT="${HOME}/.rbenv"
    export PATH="${RBENV_ROOT}/bin:${PATH}"
    eval "$(rbenv init -)"
  EOF
end

def setup_rbenv
  system 'curl https://raw.github.com/fesplugas/rbenv-installer/master/bin/rbenv-installer | bash'

  puts %Q(Adding rbenv path to "#{File.expand_path('~/', '.zshrc')}")
  system <<-EOF
    #{init_rbenv}

    grep "rbenv init -" ${HOME}/.zshrc
    if [ $? -ne 0 ]; then
      echo '
export RBENV_ROOT="${HOME}/.rbenv"

if [ -d "${RBENV_ROOT}" ]; then
  export PATH="${RBENV_ROOT}/bin:${PATH}"
  eval \"$(rbenv init -)"
fi ' >> ${HOME}/.zshrc
fi
  EOF
end

def install_ruby
  ruby_version = '2.0.0-p195'

  puts %Q(==> Installing ruby "#{ruby_version} and setting as global")
  system <<-EOF
    #{init_rbenv}

    rbenv install #{ruby_version}
    rbenv global  #{ruby_version}
  EOF
end

def install_gems
  system <<-EOF
    #{init_rbenv}

    gem install awesome_print
    gem install bundler
    gem install hirb
    gem install interactive_editor
    gem install ruby_clone
  EOF
end
