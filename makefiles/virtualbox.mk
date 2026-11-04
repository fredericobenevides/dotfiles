virtualbox-all:
	@command -v VBoxManage > /dev/null 2>&1 && echo "#### VirtualBox already installed!" || $(MAKE) virtualbox-install

virtualbox-install:
	@echo "#### Installing and configuring VirtualBox"
	sudo pacman -S --noconfirm --needed virtualbox virtualbox-host-dkms
	sudo usermod -aG vboxusers $(USER)
	sudo modprobe vboxdrv
	@echo "#### Log out and back in for the vboxusers group to take effect."
