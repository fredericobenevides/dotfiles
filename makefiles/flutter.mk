FLUTTER_DIR := $(HOME)/.flutter
FLUTTER_TMP := /tmp/flutter_release.tar.xz
FLUTTER_URL := https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_3.24.1-stable.tar.xz

flutter-all:
	@test -d $(FLUTTER_DIR) && echo "#### Flutter already configured!" || $(MAKE) flutter-setup

flutter-setup:
	@echo "#### Installing and configuring flutter"

	@echo "-- Downloading flutter"
	curl -o $(FLUTTER_TMP) -L $(FLUTTER_URL)

	@echo "-- Creating the directory $(FLUTTER_DIR)"
	mkdir -p $(FLUTTER_DIR)

	@echo "-- Extracting flutter to $(FLUTTER_DIR)"
	tar -xf $(FLUTTER_TMP) -C $(FLUTTER_DIR) --strip-components=1

	@echo "-- Cleaning up temporary file"
	rm $(FLUTTER_TMP)