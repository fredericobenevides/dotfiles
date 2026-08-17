CLOUDFLARE_PKG = cloudflare-warp-bin

cloudflare-all:
	@pacman -Qs $(CLOUDFLARE_PKG) > /dev/null 2>&1 && echo "#### Cloudflare WARP already installed!" || $(MAKE) cloudflare-install

cloudflare-install:
	@echo "#### Installing Cloudflare WARP from AUR"
	yay -S --noconfirm --needed $(CLOUDFLARE_PKG)

	@echo "#### Enabling and starting warp-svc"
	sudo systemctl enable --now warp-svc
