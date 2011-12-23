echo "\n\n####################"
echo "\tHomebrew"
echo "####################\n\n"

echo "Changing permission /usr/local to `whoami`"
sudo chown -R `whoami` /usr/local

echo "Creating the folder /usr/local/homebrew and downloading homebrew"
mkdir -p /usr/local/homebrew 
curl -L https://github.com/mxcl/homebrew/tarball/master | tar xz --strip 1 -C /usr/local/homebrew

echo "Creating the link 'brew' in /usr/local/bin/brew"
ln -sf /usr/local/homebrew/bin/brew /usr/local/bin/brew