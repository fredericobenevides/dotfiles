HYPR_CONFIG_DIR := $(HOME)/.config/hypr
HYPR_DOTFILES   := $(HOME)/.dotfiles/files/hyprland

HYPR_PKGS := wayland waybar hyprland hyprpicker rofi-wayland grim slurp imv swayidle swaync

hyprland-all:
	@pacman -Q hyprland > /dev/null 2>&1 && test -d $(HYPR_CONFIG_DIR) && echo "#### Hyprland already configured!" || $(MAKE) hyprland-setup

hyprland-setup:
	@echo "#### Installing and configuring hyprland"
	
	@echo "-- Installing system packages"
	sudo pacman -S --needed --noconfirm $(HYPR_PKGS)
	
	@echo "-- Installing AUR packages"
	yay -S --needed --noconfirm swaylock-effects-git
	
	@echo "-- Creating directory $(HYPR_CONFIG_DIR)"
	mkdir -p $(HYPR_CONFIG_DIR)
	
	@echo "-- Linking files"
	ln -sf $(HYPR_DOTFILES)/hyprland.conf $(HYPR_CONFIG_DIR)/hyprland.conf