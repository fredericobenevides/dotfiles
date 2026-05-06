HYPR_CONFIG_DIR := $(HOME)/.config/hypr

HYPR_PKGS := wayland waybar hyprland hypridle hyprpicker hyprlock rofi-wayland grim slurp imv swaync brightnessctl

hyprland-all:
	@pacman -Q hyprland > /dev/null 2>&1 && test -d $(HYPR_CONFIG_DIR) && echo "#### Hyprland already configured!" || $(MAKE) hyprland-setup

hyprland-setup:
	@echo "#### Installing and configuring hyprland"
	
	@echo "-- Installing system packages"
	sudo pacman -S --needed --noconfirm $(HYPR_PKGS)
	
	@echo "-- Creating directory $(HYPR_CONFIG_DIR)"
	mkdir -p $(HYPR_CONFIG_DIR)
	
	@echo "-- Linking files"
	@$(foreach file,$(shell ls $(DOTS_DIR)/.config/hypr),ln -sf $(DOTS_DIR)/.config/hypr/$(file) $(HYPR_CONFIG_DIR)/$(file);)