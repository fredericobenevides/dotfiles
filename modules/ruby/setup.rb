def install_module
  install 'RUBY' do
    description 'Linking configs files for gems and irb'
    link from: 'ruby/gemrc', to: '~/', make_hidden: true
    link from: 'ruby/irbrc', to: '~/', make_hidden: true

    install_rbenv

    install_ruby_with_rbenv '2.2.2'
    install_gems
  end
end

def uninstall_module
  uninstall 'RUBY' do
    description 'Unlinking configs files for gems and irb'
    unlink from: 'ruby/gemrc', to: '~/', make_hidden: true
    unlink from: 'ruby/irbrc', to: '~/', make_hidden: true

    description 'Removing the rbenv folder'
    run 'rm -rf ~/.rbenv'

    description 'Removing the gem folder'
    run 'rm -rf ~/.gem'

    description %Q(Removing rbenv path from "#{File.join(File.expand_path('~/'), '.zshrc')}")
    remove_rbenv_zsh = "gsed -i '/export RBENV/,+5d' ~/.zshrc"

    when_os :mac do
      run remove_rbenv_zsh
    end

    when_os :linux do
      run remove_rbenv_zsh.gsub('gsed', 'sed')
    end
  end
end

def install_rbenv
  description 'Installing rbenv using rbenv-installer'
  run 'curl https://raw.githubusercontent.com/fesplugas/rbenv-installer/master/bin/rbenv-installer | bash'
  add_rbenv_to_zsh
end

def add_rbenv_to_zsh
  command = <<-EOF
grep "rbenv init -" ${HOME}/.zshrc
if [ $? -ne 0 ]; then

  echo '
export RBENV_ROOT="${HOME}/.rbenv"

if [ -d "${RBENV_ROOT}" ]; then
  export PATH="${RBENV_ROOT}/bin:${PATH}"
  eval \"$(rbenv init -)"
fi' >> ${HOME}/.zshrc

fi
  EOF

  description %(Adding rbenv path to "#{File.join(File.expand_path('~/'), '.zshrc')}")
  run command
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
  EOF

  description 'Installing default gems'
  run command
end
