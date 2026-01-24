ROFI_CONFIG_DIR := $(HOME)/.config/rofi
ROFI_DOTFILES   := $(HOME)/.dotfiles/files/rofi

ROFI_FILES := config.rasi \
              power-menu.rasi \
              power-profiles.rasi \
              rofikeyhint.rasi \
              rofidmenu.rasi

rofi-all:
	@test -d $(ROFI_CONFIG_DIR) && echo "#### Rofi already configured!" || $(MAKE) rofi-setup

rofi-setup:
	@echo "#### Configuring rofi"
	
	@echo "-- Creating directory $(ROFI_CONFIG_DIR)"
	@mkdir -p $(ROFI_CONFIG_DIR)
	
	@echo "-- Linking configuration files"
	@$(foreach file, $(ROFI_FILES), ln -sf $(ROFI_DOTFILES)/$(file) $(ROFI_CONFIG_DIR)/$(file);)