ZSH_DIR         := $(HOME)/.oh-my-zsh

ZSH_PLUGINS_DIR := $(ZSH_DIR)/custom/plugins
ZSH_PLUGINS     := fredericobenevides myutils

zsh-all: zsh-install zsh-pkgs

zsh-install:
	@test -d $(ZSH_DIR) && echo "#### Zsh (Oh My Zsh) already configured!" || $(MAKE) zsh-setup

zsh-setup:
	@echo "#### Installing Oh My Zsh"
	@CHSH=no RUNZSH=no sh -c "$$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

zsh-pkgs:
	@echo "#### Configuring Zsh plugins, themes and rc"
	
	@echo "-- Linking .zshrc"
	@ln -sf $(DOTS_DIR)/.zshrc $(HOME)/.zshrc
	
	@echo "-- Linking custom plugins"
	@mkdir -p $(ZSH_PLUGINS_DIR)
	@$(foreach plugin, $(ZSH_PLUGINS), ln -sfn $(DOTS_DIR)/.oh-my-zsh/custom/plugins/$(plugin) $(ZSH_PLUGINS_DIR)/$(plugin);)
	
	@echo "-- Linking custom theme"
	@ln -sf $(DOTS_DIR)/.oh-my-zsh/themes/fredericobenevides.zsh-theme $(ZSH_DIR)/themes/fredericobenevides.zsh-theme