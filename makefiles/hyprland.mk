HYPR_CONFIG_DIR := $(HOME)/.config/hypr

HYPR_PKGS := wayland hyprcursor hyprland hypridle hyprlock hyprpicker awww grim slurp imv brightnessctl wl-clipboard cliphist gammastep

hyprland-all:
	@pacman -Q hyprland > /dev/null 2>&1 && test -d $(HYPR_CONFIG_DIR) && echo "#### Hyprland already configured!" || $(MAKE) hyprland-setup hyprland-pkgs

hyprland-setup:
	@echo "#### Installing and configuring hyprland"
	
	@echo "-- Removing old link configuration directory if it exists"
	@rm -rf $(HYPR_CONFIG_DIR)
	
	@echo "-- Linking configuration files"
	ln -sf $(DOTS_DIR)/.config/hypr/ $(HYPR_CONFIG_DIR)

hyprland-pkgs:
	@echo "-- Installing system packages"
	sudo pacman -S --needed --noconfirm $(HYPR_PKGS)
	
