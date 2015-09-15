def install_module
  install 'RUBY' do
    description 'Linking configs files for gems and irb'
    link from: 'ruby/files/gemrc', to: '~/', make_hidden: true
    link from: 'ruby/files/irbrc', to: '~/', make_hidden: true

    install_rbenv
    add_rbenv_to_zsh

    install_ruby_with_rbenv '2.2.2'
    install_gems
  end
end

def uninstall_module
  uninstall 'RUBY' do
    description 'Unlinking configs files for gems and irb'
    unlink from: 'ruby/files/gemrc', to: '~/', make_hidden: true
    unlink from: 'ruby/files/irbrc', to: '~/', make_hidden: true

    description 'Unlinking the rbenv.zsh to .zsh configs folder'
    unlink from: 'ruby/files/rbenv.zsh', to: '~/.zsh/configs/rbenv.zsh'

    description 'Removing the rbenv folder'
    run 'rm -rf ~/.rbenv'

    description 'Removing the gem folder'
    run 'rm -rf ~/.gem'
  end
end

def install_rbenv
  description 'Installing rbenv using rbenv-installer'
  run 'curl https://raw.githubusercontent.com/fesplugas/rbenv-installer/master/bin/rbenv-installer | bash'
end

def add_rbenv_to_zsh
  description 'Linking the rbenv.zsh to .zsh configs folder'
  link from: 'ruby/files/rbenv.zsh', to: '~/.zsh/configs/rbenv.zsh'
end

def run_rbenv
  <<-EOF
export RBENV_ROOT="${HOME}/.rbenv"
export PATH="${RBENV_ROOT}/bin:${PATH}"
eval "$(rbenv init -)"
  EOF
end

def install_ruby_with_rbenv(ruby_version)
  command = <<-EOF
#{run_rbenv}

rbenv install #{ruby_version}
rbenv global  #{ruby_version}
  EOF

  description %(Installing ruby "#{ruby_version} and setting as global")
  run command
end

def install_gems
  command = <<-EOF
#{run_rbenv}

gem install awesome_print
gem install bundler
gem install hirb
gem install interactive_editor
gem install rails
gem install rubocop
gem install ruby_clone
gem install tmuxinator
  EOF

  description 'Installing default gems'
  run command
end
