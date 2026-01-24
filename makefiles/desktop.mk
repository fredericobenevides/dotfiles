DESKTOP_DIR := $(HOME)/.local/share/applications
IDEA_DESKTOP := $(DESKTOP_DIR)/idea.desktop

desktop-all:
	@test -f $(IDEA_DESKTOP) && echo "#### Desktop files already configured!" || $(MAKE) desktop-install

desktop-install:
	@echo "#### Installing and configuring desktop files"

	@echo "-- Creating desktop application's directory"
	mkdir -p $(DESKTOP_DIR)

	@echo "-- Copying desktop files"
	cp $(HOME)/.dotfiles/files/desktop/idea.desktop $(IDEA_DESKTOP)

	@echo "-- Updating the right username in desktop file"
	sed -i "s|{{USER}}|$(USER)|" $(IDEA_DESKTOP)