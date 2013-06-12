def setup_git
  if OS.mac?
    system 'brew install git'
  else
    system 'sudo apt-get install git-core'
  end

  copy_file :from => 'git/.gitconfig', :to => '~/'
  copy_file :from => 'git/.gitignore', :to => '~/'
end