def install_module
  install 'GOLANG' do
    description 'Installing golang'

    when_os :mac do
      run 'brew install go'
    end

    when_os :linux do
      run 'sudo apt-get install golang'
    end
  end
end

def uninstall_module
  uninstall 'GOLANG' do
    description 'Uninstalling golang'

    when_os :mac do
      run 'brew uninstall go'
    end

    when_os :linux do
      run 'sudo apt-get purge golang'
    end
  end
end
