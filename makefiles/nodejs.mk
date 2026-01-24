FNM_DIR      := $(HOME)/.fnm
NODE_VERSION := v24.13.0
NODE_BIN     := $(FNM_DIR)/fnm

FNM_ENV := export PATH=$(FNM_DIR):$(PATH) && eval "$$($(NODE_BIN) env)"

NPM_PKGS := pnpm

nodejs-all: nodejs-install nodejs-pkgs

nodejs-install:
	@test -d $(FNM_DIR) && echo "#### Nodejs (fnm) already configured!" || $(MAKE) nodejs-setup

nodejs-setup:
	@echo "#### Installing fnm (Fast Node Manager)"
	curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir $(FNM_DIR) --skip-shell
	
	@echo "-- Installing Node.js $(NODE_VERSION)"
	@$(FNM_ENV) && fnm install $(NODE_VERSION) && fnm default $(NODE_VERSION)
	
	@echo "-- Configuring npm global directory and .npmrc"
	mkdir -p $(HOME)/.npm-global
	ln -sf $(HOME)/.dotfiles/files/npm/.npmrc $(HOME)/.npmrc

nodejs-pkgs:
	@echo "#### Installing global npm packages"
	@$(FNM_ENV) && npm install -g $(NPM_PKGS)