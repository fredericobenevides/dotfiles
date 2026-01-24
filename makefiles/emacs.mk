EMACS_DIR      := $(HOME)/.emacs.d
EMACS_DOTFILES := $(HOME)/.dotfiles/files/emacs

EMACS_FILES    := early-init.el \
                  init.el \
                  init.org

emacs-all: emacs-install emacs-pkgs

emacs-install:
	@pacman -Q emacs > /dev/null 2>&1 && echo "#### Emacs already installed!" || $(MAKE) emacs-setup

emacs-setup:
	@echo "#### Installing Emacs"
	sudo pacman -S --needed --noconfirm emacs

emacs-pkgs:
	@echo "#### Configuring Emacs (linking files)"
	@mkdir -p $(EMACS_DIR)
	@$(foreach file, $(EMACS_FILES), ln -sf $(EMACS_DOTFILES)/$(file) $(EMACS_DIR)/$(file);)