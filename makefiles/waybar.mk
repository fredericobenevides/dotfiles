WAYBAR_CONFIG_DIR := $(HOME)/.config/waybar

WAYBAR_FILES := config \
                style.css

waybar-all:
	@test -d $(WAYBAR_CONFIG_DIR) && echo "#### Waybar already configured!" || $(MAKE) waybar-setup

waybar-setup:
	@echo "#### Configuring waybar"
	@mkdir -p $(WAYBAR_CONFIG_DIR)
	@echo "-- Linking $(words $(WAYBAR_FILES)) configuration files"
	@$(foreach file, $(WAYBAR_FILES), ln -sf $(DOTS_DIR)/.config/waybar/$(file) $(WAYBAR_CONFIG_DIR)/$(file);)