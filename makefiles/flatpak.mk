FLATPAK_PKGS := com.google.AndroidStudio \
				com.usebruno.Bruno \
                com.google.Chrome \
                com.spotify.Client \
                com.github.tchx84.Flatseal \
                rest.insomnia.Insomnia \
				dev.vencord.Vesktop


flatpak-all:  flatpak-install flatpak-pkgs

flatpak-install:
	@pacman -Q flatpak > /dev/null 2>&1 && echo "#### Flatpak saetup already configured!" || $(MAKE) flatpak-setup

flatpak-setup:
	@echo "#### Installing flatpak"
	sudo pacman -S --needed --noconfirm flatpak

	@echo "-- Installing portals for Hyprland/Wayland"
	sudo pacman -S --needed --noconfirm xdg-desktop-portal-hyprland xdg-desktop-portal-gtk

	@echo "-- Adding the Flathub repository"
	flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

flatpak-pkgs:
	@echo "#### Installing flatpak packages"
	flatpak install -y flathub $(FLATPAK_PKGS)
