NVIM_CONFIG_DIR := $(HOME)/.config/nvim
NVIM_SITE_DIR   := $(HOME)/.local/share/nvim/site
NVIM_DOTFILES   := $(HOME)/.dotfiles/files/vim

neovim-all: neovim-install neovim-pkgs

neovim-install:
	@pacman -Q neovim > /dev/null 2>&1 && echo "#### Neovim already installed!" || $(MAKE) neovim-setup

neovim-setup:
	@echo "#### Installing Neovim"
	sudo pacman -S --needed --noconfirm neovim

neovim-pkgs:
	@echo "#### Configuring Neovim environment"
	@mkdir -p $(NVIM_CONFIG_DIR)
	@mkdir -p $(HOME)/.vim
	@mkdir -p $(NVIM_SITE_DIR)/autoload
	@mkdir -p $(NVIM_SITE_DIR)/pack/themes/start

	@echo "-- Installing vim-plug"
	@curl -fLo $(NVIM_SITE_DIR)/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

	@echo "-- Linking configuration files"
	@ln -sf $(NVIM_DOTFILES)/.vimrc $(HOME)/.vimrc
	@ln -sf $(NVIM_DOTFILES)/.vimrc $(NVIM_CONFIG_DIR)/init.vim
	@ln -sf $(NVIM_DOTFILES)/coc-settings.json $(NVIM_CONFIG_DIR)/coc-settings.json
	
	@echo "Tip: Run :PlugInstall inside Neovim to install plugins."