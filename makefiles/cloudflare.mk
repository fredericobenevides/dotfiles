CLOUDFLARE_PKG = cloudflare-warp-bin

cloudflare-all:
	@pacman -Qs $(CLOUDFLARE_PKG) > /dev/null 2>&1 && echo "#### Cloudflare WARP already installed!" || $(MAKE) cloudflare-install

cloudflare-install:
	@echo "#### Installing Cloudflare WARP from AUR"
	@echo -e "\033[0;31m⚠ WARNING: AUR packages are not officially supported. Review the PKGBUILD before proceeding:\033[0m"
	@echo -e "\033[0;31m  https://aur.archlinux.org/packages/$(CLOUDFLARE_PKG)\033[0m"
	@read -p "Continue? (y/N) " confirm && [ "$$confirm" = "y" ] || exit 1
	yay -S --needed --noconfirm $(CLOUDFLARE_PKG)

	@echo "#### Enabling and starting warp-svc"
	sudo systemctl enable --now warp-svc
