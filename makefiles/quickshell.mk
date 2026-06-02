QUICKSHELL_CONFIG_DIR := $(HOME)/.config/quickshell

QUICKSHELL_PKGS := quickshell-git qt6-5compat

quickshell-all:
	@pacman -Q quickshell > /dev/null 2>&1 && test -d $(QUICKSHELL_CONFIG_DIR) && echo "#### Quickshell already configured!" || $(MAKE) quickshell-setup quickshell-pkgs

quickshell-setup:
	@echo "#### Installing and configuring quickshell"

	@echo "-- Removing old link configuration directory if it exists"
	@rm -rf $(QUICKSHELL_CONFIG_DIR)

	@echo "-- Linking configuration files"
	ln -sf $(DOTS_DIR)/.config/quickshell/ $(QUICKSHELL_CONFIG_DIR)

quickshell-pkgs:
	@echo "-- Installing quickshell packages"
	sudo pacman -S --needed --noconfirm $(QUICKSHELL_PKGS)
