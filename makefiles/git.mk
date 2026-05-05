git-all:
	@test -f $(HOME)/.gitconfig && test -f $(HOME)/.gitignore && echo "#### Git already configured!" || $(MAKE) git-setup

git-setup:
	@echo "#### Configuring git"
	
	@echo "-- Linking files"
	ln -sf $(DOTS_DIR)/.gitconfig $(HOME)/.gitconfig
	ln -sf $(DOTS_DIR)/.gitignore $(HOME)/.gitignore