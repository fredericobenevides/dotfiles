def install_ag_silver_searcher
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

def uninstall_ag_silver_searcher
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
