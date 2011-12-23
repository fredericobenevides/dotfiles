echo "\n\n####################"
echo "\tGIT"
echo "####################\n\n"

echo "Installing git using Homebrew"
brew install git

echo "Copying the git files to $HOME as hidden files"
cp gitconfig ~/.gitconfig
cp gitignore ~/.gitignore