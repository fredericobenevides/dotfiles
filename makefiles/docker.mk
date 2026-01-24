docker-all:
	@groups | grep -q docker 2>&1 && echo "#### Docker already configured!" || $(MAKE) docker-install

docker-install:
	@echo "#### Installing and configuring docker"
	sudo pacman -S --noconfirm --needed docker docker-compose
	sudo systemctl enable --now docker.service
	sudo usermod -aG docker $(USER)