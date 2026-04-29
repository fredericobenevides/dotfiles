PACMAN_PKGS := alacritty \
		cmake \
		conky \
		flameshot \
		gdb \
		vivaldi \
		xdotool \
		zathura-pdf-mupdf


pacman-all:
	@echo "#### Installing pacman packages"
	sudo pacman -S --needed --noconfirm $(PACMAN_PKGS)
