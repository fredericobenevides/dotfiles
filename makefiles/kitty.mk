KITTY_CONFIG_DIR := $(HOME)/.config/kitty

KITTY_PKGS := kitty

kitty-all:
	@pacman -Q kitty > /dev/null 2>&1 && test -d $(KITTY_CONFIG_DIR) && echo "#### Kitty already configured!" || $(MAKE) kitty-setup

kitty-setup:
	@echo "#### Installing and configuring kitty"
	
	@echo "-- Removing old link configuration directory if it exists"
	@rm -rf $(KITTY_CONFIG_DIR)
	
	@echo "-- Linking configuration files"
	ln -sf $(DOTS_DIR)/.config/kitty/ $(KITTY_CONFIG_DIR)

