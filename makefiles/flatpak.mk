FLATPAK_GUI_PKGS := com.google.Chrome \
                    com.github.tchx84.Flatseal \
                    com.spotify.Client \
                    dev.vencord.Vesktop

FLATPAK_DEV_PKGS := com.google.AndroidStudio \
                    com.usebruno.Bruno \
                    rest.insomnia.Insomnia


flatpak-all: flatpak-install flatpak-gui-all flatpak-dev-all

flatpak-install:
	@pacman -Q flatpak > /dev/null 2>&1 && echo "#### Flatpak setup already configured!" || $(MAKE) flatpak-setup

flatpak-setup:
	@echo "#### Installing flatpak"
	sudo pacman -S --needed --noconfirm flatpak

	@echo "-- Installing portals for Hyprland/Wayland"
	sudo pacman -S --needed --noconfirm xdg-desktop-portal-hyprland xdg-desktop-portal-gtk

	@echo "-- Adding the Flathub repository"
	flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

flatpak-gui-all: flatpak-install
	@echo "#### Installing flatpak GUI packages"
	flatpak install -y flathub $(FLATPAK_GUI_PKGS)

flatpak-dev-all: flatpak-install
	@echo "#### Installing flatpak dev packages"
	flatpak install -y flathub $(FLATPAK_DEV_PKGS)
