FZF_DIR := $(HOME)/.fzf

fzf-all:
	@test -d $(FZF_DIR) && echo "#### fzf already configured!" || $(MAKE) fzf-setup

fzf-setup:
	@echo "#### Installing and configuring fzf"
	
	@echo "-- Cloning fzf from git"
	git clone --depth 1 https://github.com/junegunn/fzf.git $(FZF_DIR)
	
	@echo "-- Running install script from fzf"
	# --all ativa o auto-completion, key-bindings e atualiza o seu .zshrc/.bashrc sem perguntar
	$(FZF_DIR)/install --all