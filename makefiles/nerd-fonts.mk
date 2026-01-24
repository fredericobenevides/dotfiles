FONTS_DIR := $(HOME)/.local/share/fonts/NerdFonts/JetBrainsMono
FONTS_TMP := /tmp/JetBrainsMono.tar.xz
FONTS_URL := https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/JetBrainsMono.tar.xz

nerd-fonts-all:
	@test -d $(FONTS_DIR) && echo "#### Nerd Fonts already configured!" || $(MAKE) nerd-fonts-setup

nerd-fonts-setup:
	@echo "#### Configuring Nerd Fonts in the system"

	@echo "-- Downloading JetBrainsMono"
	curl -o $(FONTS_TMP) -L $(FONTS_URL)

	@echo "-- Creating the directory $(FONTS_DIR)"
	mkdir -p $(FONTS_DIR)

	@echo "-- Extracting font files"
	tar -xf $(FONTS_TMP) -C $(FONTS_DIR)

	@echo "-- Reloading font cache"
	fc-cache -f

	@echo "-- Cleaning up temporary file"
	rm $(FONTS_TMP)