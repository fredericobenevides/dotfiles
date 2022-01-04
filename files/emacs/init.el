(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; install use-package if is not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; load use-package
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;; download automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(set-face-attribute 'default nil :font "JetBrainsMono NF" :height 160)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono NF" :height 160)
(set-face-attribute 'variable-pitch nil :font "JetBrainsMono NF" :height 160)

;; Show column and line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some mode
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-hl-line-mode 1) ;; Enable hl

(setq inhibit-startup-message t) ;; Don't show the startup message

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; maximize window

(menu-bar-mode -1)    ;; Disable menu bar
(scroll-bar-mode -1)  ;; Disable visible scrollbar
(tool-bar-mode -1)    ;; Disable the toolbar
(tooltip-mode +1)     ;; Disable tooltips

(set-fringe-mode 10)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package all-the-icons)

(use-package dashboard
  :config

  ;; list to show on dashboard
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)))

  ;; show icons
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; change banner
  (setq dashboard-startup-banner 'logo)

  ;; start dashboard
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package command-log-mode
 :config
  (global-command-log-mode)
  :bind ("C-c o" . clm/toggle-command-log-buffer))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(show-paren-mode 1)

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit)

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
