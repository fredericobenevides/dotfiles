def install_module
  install 'AG - Silver Searcher'do
    description 'Installing the fast searcher'

    when_os :mac do
      run 'brew install the_silver_searcher'
    end

    when_os :linux do
      run 'sudo apt-get install silversearcher-ag'
    end
  end
end

def uninstall_module
  uninstall 'AG - Silver Searcher'do
    description 'Installing the fast searcher'

    when_os :mac do
      run 'brew uninstall the_silver_searcher'
    end

    when_os :linux do
      run 'sudo apt-get purge silversearcher-ag'
    end
  end
end
