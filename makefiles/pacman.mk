PACMAN_PKGS := alacritty \
		cmake \
		conky \
		flameshot \
		gdb \
		xdotool \
		zathura-pdf-mupdf


pacman-all:
	@echo "#### Installing pacman packages"
	sudo pacman -S --needed --noconfirm $(PACMAN_PKGS)
