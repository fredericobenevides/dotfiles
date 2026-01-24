X11_DOTFILES := $(HOME)/.dotfiles/files/x11

x11-all:
	@test -f $(HOME)/.XCompose && echo "#### X11 (Compose) already configured!" || $(MAKE) x11-setup

x11-setup:
	@echo "#### Configuring X11 Compose"
	
	@echo "-- Linking .XCompose file"
	ln -sf $(X11_DOTFILES)/.XCompose $(HOME)/.XCompose