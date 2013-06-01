def setup_git
  system 'brew install git' if OS.mac?
  copy_file :from => 'git/.gitconfig', :to => '~/'
  copy_file :from => 'git/.gitignore', :to => '~/'
end