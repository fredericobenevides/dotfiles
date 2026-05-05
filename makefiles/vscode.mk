VSCODE_CONFIG_DIR := $(HOME)/.config/Code/User

VSCODE_EXTS := Catppuccin.catppuccin-vsc \
               vscode-icons-team.vscode-icons \
               alefragnani.Bookmarks \
               bierner.emojisense \
               ritwickdey.LiveServer \
               usernamehw.find-jump \
               vadimcn.vscode-lldb \
               wayou.vscode-todo-highlight \
               oderwat.indent-rainbow \
               kisstkondoros.vscode-gutter-preview \
               mikestead.dotenv \
               donjayamanne.githistory \
               eamodio.gitlens \
               ms-azuretools.vscode-docker \
               golang.Go \
               rust-lang.rust-analyzer \
               tamasfe.even-better-toml \
               esbenp.prettier-vscode \
               fill-labs.dependi \
               formulahendry.auto-rename-tag \
               dbaeumer.vscode-eslint \
               xabikos.JavaScriptSnippets \
               bradlc.vscode-tailwindcss \
               kamikillerto.vscode-colorize \
               pranaygp.vscode-css-peek

vscode-all: vscode-install vscode-pkgs

vscode-install:
	@pacman -Qi code > /dev/null 2>&1 && echo "#### VSCode already installed!" || $(MAKE) vscode-setup

vscode-setup:
	@echo "#### Installing VSCode (bin)"
	yay -S --needed --noconfirm visual-studio-code-bin

vscode-pkgs:
	@echo "#### Configuring VSCode (linking settings)"
	@mkdir -p $(VSCODE_CONFIG_DIR)
	@ln -sf $(DOTS_DIR)/.config/Code/User/settings.json $(VSCODE_CONFIG_DIR)/settings.json
	@ln -sf $(DOTS_DIR)/.config/Code/User/keybindings.json $(VSCODE_CONFIG_DIR)/keybindings.json

	@echo "#### Installing VSCode extensions"
	@$(foreach ext, $(VSCODE_EXTS), code --install-extension $(ext) --force;)