GIT_DOTFILES := $(HOME)/.dotfiles/files/git

git-all:
	@test -f $(HOME)/.gitconfig && test -f $(HOME)/.gitignore && echo "#### Git already configured!" || $(MAKE) git-setup

git-setup:
	@echo "#### Configuring git"
	
	@echo "-- Linking files"
	ln -sf $(GIT_DOTFILES)/.gitconfig $(HOME)/.gitconfig
	ln -sf $(GIT_DOTFILES)/.gitignore $(HOME)/.gitignore