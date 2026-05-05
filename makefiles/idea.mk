IDEA_DIR        := $(HOME)/.idea
IDEA_TMP        := /tmp/idea_installer.tar.gz
IDEA_URL        := https://download.jetbrains.com/idea/ideaIU-2025.3.4.tar.gz
IDEA_DOTS_FILE := $(DOTS_DIR)/.local/share/applications/idea.desktop
IDEA_DESKTOP_DIR := $(HOME)/.local/share/applications
IDEA_DESKTOP     := $(IDEA_DESKTOP_DIR)/idea.desktop

idea-all:
	@test -d $(IDEA_DIR) && test -f $(IDEA_DESKTOP) && echo "#### Idea already configured!" || $(MAKE) idea-setup

idea-setup:
	@echo "#### Installing and configuring idea"

	@echo "-- Downloading intellij"
	@curl -o $(IDEA_TMP) -L $(IDEA_URL)

	@echo "-- Creating the directory $(IDEA_DIR)"
	@mkdir -p $(IDEA_DIR)

	@echo "-- Extracting intellij to $(IDEA_DIR)"
	@tar -xf $(IDEA_TMP) -C $(IDEA_DIR) --strip-components=1

	@echo "-- Configuring desktop shortcut"
	@mkdir -p $(IDEA_DESKTOP_DIR)	

	@echo "-- Updating the right username in desktop file"
	sed "s|{{USER}}|$(USER)|g" $(IDEA_DOTS_FILE) > $(IDEA_DESKTOP)

	@echo "-- Cleaning up temporary file"
	@rm $(IDEA_TMP)