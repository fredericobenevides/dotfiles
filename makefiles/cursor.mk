CURSOR_CONFIG_DIR := $(HOME)/.local/share/icons/Bibata-Modern-Ice

cursor-all:
	@test -d $(CURSOR_CONFIG_DIR) && echo "#### Cursor (Bibata-Modern-Ice) already configured!" || $(MAKE) cursor-setup

cursor-setup:
	@echo "#### Configuring cursor (Bibata-Modern-Ice)"

	@echo "-- Removing old link configuration directory if it exists"
	@rm -rf $(CURSOR_CONFIG_DIR)

	@echo "-- Creating parent directory"
	@mkdir -p $(HOME)/.local/share/icons

	@echo "-- Linking cursor files"
	ln -sf $(DOTS_DIR)/.local/share/icons/Bibata-Modern-Ice/ $(CURSOR_CONFIG_DIR)
