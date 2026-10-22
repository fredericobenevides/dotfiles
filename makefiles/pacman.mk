PACMAN_PKGS := htop

PACMAN_GUI_PKGS := alacritty \
                   conky \
                   flameshot \
                   gpu-screen-recorder \
                   shotcut \
                   vivaldi \
                   xdotool \
                   zathura-pdf-mupdf

PACMAN_DEV_PKGS := cmake \
                   gdb \
                   opencode

pacman-all:
	@echo "#### Installing pacman packages"
	sudo pacman -S --needed --noconfirm $(PACMAN_PKGS)

pacman-gui-all:
	@echo "#### Installing pacman GUI packages"
	sudo pacman -S --needed --noconfirm $(PACMAN_GUI_PKGS)

pacman-dev-all:
	@echo "#### Installing pacman dev packages"
	sudo pacman -S --needed --noconfirm $(PACMAN_DEV_PKGS)
