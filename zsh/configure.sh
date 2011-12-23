echo "\n\n####################"
echo "\tZSH"
echo "####################\n\n"

echo "Instaling OH-MY-ZSH"

git clone https://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc
echo "export PATH=$PATH" >> ~/.zshrc
chsh -s `which zsh`

echo "Copying themes to oh-my-zsh folder"
cp themes/* ~/.oh-my-zsh/themes

echo "Copying custom plugins to oh-my-zsh folder"
cp -R plugins ~/.oh-my-zsh/custom

echo "Changing the theme to 'fredericobenevides'"
cat ~/.zshrc | sed '/ZSH_THEME/ c\
ZSH_THEME="fredericobenevides"
' > ~/.zshrc_dotfiles
mv ~/.zshrc_dotfiles ~/.zshrc

echo "Changing the plugin to use: git fredericobenevides"
cat ~/.zshrc | sed '/plugins=/ c\
plugins=(brew bundler gem git fredericobenevides)
' > ~/.zshrc_dotfiles
mv ~/.zshrc_dotfiles ~/.zshrc

echo "ZSH installed."