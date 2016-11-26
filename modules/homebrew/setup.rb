def install_module
  install 'HOMEBREW' do

    when_os :mac do
      run %(ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)")

      username = `whoami`.chomp
      description "Changing the permission folder of /usr/local to the owner #{username}"
      run "sudo chown -R #{username} /usr/local"

      description 'Installing the gnu-sed that is the same sed for linux!'
      run 'brew install gnu-sed'

      description 'Installing brew cask'
      run 'brew install caskroom/cask/brew-cask'

      description 'Installing iterm2'
      run 'brew cask install iterm2'
    end

    when_os :linux do
      description '******** Skipping HOMEBREW for linux ********'
    end
  end
end

def uninstall_module
  uninstall 'HOMEBREW' do
    when_os :mac do
      description 'Removing the folder /usr/local created by homebrew'
      run 'sudo rm -rf /usr/local'
    end

    when_os :linux do
      description '******** Skipping HOMEBREW for linux ********'
    end
  end
end
