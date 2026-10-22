VSCODE_CONFIG_DIR := $(HOME)/.config/Code/User

# --- Appearance (theme and icons) ---
VSCODE_EXTS_UI := Catppuccin.catppuccin-vsc \
                   vscode-icons-team.vscode-icons

# --- Editor and productivity ---
VSCODE_EXTS_EDITOR := alefragnani.Bookmarks \
                       bierner.emojisense \
                       formulahendry.auto-rename-tag \
                       kamikillerto.vscode-colorize \
                       mikestead.dotenv \
                       oderwat.indent-rainbow \
                       usernamehw.find-jump \
                       wayou.vscode-todo-highlight

# --- Languages and frameworks ---
VSCODE_EXTS_LANG := bbenoist.QML \
                     bradlc.vscode-tailwindcss \
                     golang.Go \
                     GraphQL.vscode-graphql \
                     humao.rest-client \
                     rust-lang.rust-analyzer \
                     xabikos.JavaScriptSnippets

# --- Lint and formatting ---
VSCODE_EXTS_LINT := dbaeumer.vscode-eslint \
                     esbenp.prettier-vscode \
                     tamasfe.even-better-toml

# --- Infra: docker, make and debug ---
VSCODE_EXTS_INFRA := ms-azuretools.vscode-docker \
                      ms-vscode.makefile-tools \
                      vadimcn.vscode-lldb

# --- Git and dependencies ---
VSCODE_EXTS_GIT := donjayamanne.githistory \
                    eamodio.gitlens \
                    fill-labs.dependi

# --- Preview and servers ---
VSCODE_EXTS_PREVIEW := kisstkondoros.vscode-gutter-preview \
                        pranaygp.vscode-css-peek \
                        ritwickdey.LiveServer

# --- Markdown ---
VSCODE_EXTS_MARKDOWN := DavidAnson.vscode-markdownlint \
                         shd101wyy.markdown-preview-enhanced \
                         yzhang.markdown-all-in-one

# --- AI ---
VSCODE_EXTS_AI := sst-dev.opencode

# --- Optional (installed separately, heavy) ---
VSCODE_EXTS_OPTIONAL := GoogleCloudTools.cloudcode

VSCODE_EXTS := $(VSCODE_EXTS_UI) \
               $(VSCODE_EXTS_EDITOR) \
               $(VSCODE_EXTS_LANG) \
               $(VSCODE_EXTS_LINT) \
               $(VSCODE_EXTS_INFRA) \
               $(VSCODE_EXTS_GIT) \
               $(VSCODE_EXTS_PREVIEW) \
               $(VSCODE_EXTS_MARKDOWN) \
               $(VSCODE_EXTS_AI)

vscode-all: vscode-install vscode-pkgs

vscode-install:
	@pacman -Q code > /dev/null 2>&1 && echo "#### VSCode already installed!" || $(MAKE) vscode-setup

vscode-setup:
	@echo "#### Installing VSCode (bin)"
	@echo -e "\033[0;31m⚠ WARNING: AUR packages are not officially supported. Review the PKGBUILD before proceeding:\033[0m"
	@echo -e "\033[0;31m  https://aur.archlinux.org/packages/visual-studio-code-bin\033[0m"
	@read -p "Continue? (y/N) " confirm && [ "$$confirm" = "y" ] || exit 1
	yay -S --needed --noconfirm visual-studio-code-bin

vscode-pkgs:
	@echo "#### Configuring VSCode (linking settings)"
	@mkdir -p $(VSCODE_CONFIG_DIR)

	@$(foreach file,$(wildcard $(DOTS_DIR)/.config/Code/User/*),ln -sf $(file) $(VSCODE_CONFIG_DIR)/$(notdir $(file));)

	@echo "#### Installing VSCode extensions"
	@$(foreach ext, $(VSCODE_EXTS), code --install-extension $(ext) --force;)

vscode-optional:
	@echo "#### Installing optional VSCode extensions"
	@$(foreach ext, $(VSCODE_EXTS_OPTIONAL), code --install-extension $(ext) --force;)
