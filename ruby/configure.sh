echo "\n\n####################"
echo "\tRuby"
echo "####################\n\n"

echo "Installing rbenv and ruby-build"
brew install rbenv
brew install ruby-build

echo "Copying gemrc to $HOME/.gemrc"
cp gemrc ~/.gemrc

echo "Copying .irbrc to $HOME/.irbrc"
cp irbrc ~/.irbrc

echo "Adding rbenv to .zshrc"
echo 'eval "$(rbenv init -)"' >> ~/.zshrc

echo "\n\n----------------------------------------------------------------------"
echo "Rbenv was installed with NO specific ruby version. You need manually 
decide which version to install."
echo "----------------------------------------------------------------------"