#!/bin/bash

# This script was created for linux system that doesn't come with git and ruby installed

echo "Installing Git"
sudo apt-get install git-core

echo "Installing rbenv-installer"
curl https://raw.githubusercontent.com/fesplugas/rbenv-installer/master/bin/rbenv-installer | bash

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
rbenv install 2.2.2
rbenv global  2.2.2

./install.rb
