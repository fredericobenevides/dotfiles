KEYBOARD_DOTFILES := $(HOME)/.dotfiles/files/keyboard

keyboard-all:
	@test -f $(HOME)/.XCompose && echo "#### Keyboard (Compose) already configured!" || $(MAKE) keyboard-setup

keyboard-setup:
	@echo "#### Configuring keyboard"

	@echo "#### Installing fcitx5"
	sudo pacman -S --needed --noconfirm fcitx5

	@echo "-- Linking .XCompose file"
	ln -sf $(KEYBOARD_DOTFILES)/.XCompose $(HOME)/.XCompose