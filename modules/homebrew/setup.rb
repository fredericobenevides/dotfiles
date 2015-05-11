def install_homebrew
  install 'HOMEBREW' do

    when_os OS.mac? do
      run %(ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)")

      username = `whoami`.chomp
      description "Changing the permission folder of /usr/local to the owner #{username}"
      run "sudo chown -R #{username} /usr/local"

      description 'Installing the gnu-sed that is the same sed for linux!'
      run 'brew install gnu-sed'
    end

    when_os OS.linux? do
      description '******** Skipping HOMEBREW for linux ********'
    end
  end
end

def uninstall_homebrew
  uninstall 'HOMEBREW' do
    when_os OS.mac? do
      description 'Removing the folder /usr/local created by homebrew'
      run 'sudo rm -rf /usr/local'
    end

    when_os OS.linux? do
      description '******** Skipping HOMEBREW for linux ********'
    end
  end
end
