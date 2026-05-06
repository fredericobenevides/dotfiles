WAYBAR_CONFIG_DIR := $(HOME)/.config/waybar

WAYBAR_FILES := config \
                style.css

waybar-all:
	@test -d $(WAYBAR_CONFIG_DIR) && echo "#### Waybar already configured!" || $(MAKE) waybar-setup

waybar-setup:
	@echo "#### Configuring waybar"
	
	@echo "-- Removing old link configuration directory if it exists"
	@rm -rf $(WAYBAR_CONFIG_DIR)
	
	@echo "-- Linking configuration files"
	ln -sf $(DOTS_DIR)/.config/waybar/ $(WAYBAR_CONFIG_DIR)
