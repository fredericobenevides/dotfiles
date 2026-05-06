ROFI_CONFIG_DIR := $(HOME)/.config/rofi

ROFI_FILES := config.rasi \
              power-menu.rasi \
              power-profiles.rasi \
              rofikeyhint.rasi \
              rofidmenu.rasi

rofi-all:
	@test -d $(ROFI_CONFIG_DIR) && echo "#### Rofi already configured!" || $(MAKE) rofi-setup

rofi-setup:
	@echo "#### Configuring rofi"
	
	@echo "-- Removing old link configuration directory if it exists"
	@rm -rf $(ROFI_CONFIG_DIR)
	
	@echo "-- Linking configuration files"
	ln -sf $(DOTS_DIR)/.config/rofi/ $(ROFI_CONFIG_DIR)