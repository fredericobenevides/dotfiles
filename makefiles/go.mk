go-all:
	@pacman -Q go > /dev/null 2>&1 && echo "#### Go already configured!" || $(MAKE) go-setup

go-setup:
	@echo "#### Installing and configuring go"
	sudo pacman -S --needed --noconfirm go