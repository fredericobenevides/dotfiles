JAVA_VERSION := 21.0.9-tem

SDK_DIR  := $(HOME)/.sdkman
SDK_INIT := source $(SDK_DIR)/bin/sdkman-init.sh

sdkman-all: sdkman-install sdkman-pkgs

sdkman-install:
	@test -d $(SDK_DIR) && echo "#### SDKMAN! already configured!" || $(MAKE) sdkman-setup

sdkman-setup:
	@echo "#### Installing SDKMAN!"
	@export SDKMAN_DIR=$(SDK_DIR) && curl -s "https://get.sdkman.io" | bash

sdkman-pkgs:
	@echo "#### Installing SDKMAN packages (Java)"
	@bash -c "$(SDK_INIT) && \
		sdk install java $(JAVA_VERSION) || true && \
		sdk default java $(JAVA_VERSION)"