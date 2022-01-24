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
(tooltip-mode -1)     ;; Disable tooltips

(set-fringe-mode 10)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq create-lockfiles nil
      make-backup-files nil)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(defun my/kill-buffer-and-window ()
  "Kill buffer and its window on quitting"
  (local-set-key (kbd "q") 'kill-buffer-and-window))
(add-hook 'comint-mode-hook #'my/kill-buffer-and-window)

(use-package avy
  :bind
  ("C-'" . avy-goto-char))

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config

  ;; http://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
           backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


  (setq company-idle-delay 0.1)
  (setq company-echo-delay 0.1)

  (global-company-mode t))

(use-package company-web
  :bind (
         ("C-:" . company-web-html)))

;; show docs when is idling in the autocomplete
(use-package company-quickhelp
  :config
  (company-quickhelp-mode))

(use-package counsel)
(use-package swiper)


(use-package ivy
  :bind (("C-s" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-h b" . counsel-descbinds)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> o" . counsel-describe-symbol)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         :map ivy-minibuffer-map
         ("C-'" . ivy-avy)) ;; allow to run ivy inside the ivy-minibuffer
  :config
  (setq ivy-initial-inputs-alist nil) ;; Removes the ^ in ivy searches

  ;; ivy recommends this settings
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)

  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons)

(use-package all-the-icons-ivy-rich
  :init
  (all-the-icons-ivy-rich-mode 1))

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

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package command-log-mode
 :config
  (global-command-log-mode)
  :bind ("C-c o" . clm/toggle-command-log-buffer))

(electric-pair-mode 1)

(use-package emmet-mode
  :after
  (web-mode css-mode)
  :config
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.

(use-package lispy)

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package impatient-mode
  :commands impatient-mode)

(show-paren-mode 1)

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package yasnippet)

(use-package yasnippet-snippets)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c l")
  :hook
    ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
    read-process-output-max (* 1024 1024)
    treemacs-space-between-root-nodes nil
    company-idle-delay 0.0
    company-minimum-prefix-length 1
    lsp-idle-delay 0.1)

  (yas-global-mode 1))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs)

(use-package magit)

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package simple-httpd
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

(use-package vterm)

(use-package vterm-toggle
  :bind
    (("<M-f12>" . vterm-toggle)
      :map vterm-mode-map
      ("M-<" . vterm-toggle-forward)
      ("M->" . vterm-toggle-backward))
  :config
  (add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                (display-buffer-reuse-window display-buffer-at-bottom)
                (direction . bottom)
                (reusable-frames . visible)
                (window-height . 0.3))))

(use-package clojure-mode
  :config
  ;; integrated with lsp
  (add-hook 'clojure-mode-hook 'lsp)
  (add-hook 'clojurescript-mode-hook 'lsp)
  (add-hook 'clojurec-mode-hook 'lsp)

  ;; enable lispy mode and deactivate eletric-pair-local-mode
  (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1) (electric-pair-local-mode -1)))
  (add-hook 'clojurescript-mode-hook (lambda () (lispy-mode 1) (electric-pair-local-mode -1)))
  (add-hook 'clojurec-mode-hook (lambda () (lispy-mode 1) (electric-pair-local-mode -1))))

(use-package cider
  :config
  ;; disable cider showing eldoc during symbol at point
  (setq cider-eldoc-display-for-symbol-at-point nil)

  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)

  ;; When there's a cider error, don't switch to the buffer
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer nil))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(defun my/compileandrun()
  (interactive)
  (save-buffer)
  (compile (concat "g++ -g -Wall -Wextra -Werror " (file-name-nondirectory (buffer-file-name)) " -o " (file-name-sans-extension   (file-name-nondirectory (buffer-file-name))) " && ./" (file-name-sans-extension  (file-name-nondirectory (buffer-file-name)))) t )
  (other-window 1)
  (end-of-buffer))

(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "<f9>") #'my/compileandrun)))

;; gdb show buffer
(setq gdb-show-main t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(defun my-markdown-preview-stop ()
  "Stop preview"
  (interactive)
  (unless (process-status "httpd")
    (httpd-stop))
  (impatient-mode -1))

(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)

  ;; integrated with lsp
  (add-hook 'web-mode-hook 'lsp)

  ;; integrated with emmet-mode
  (add-hook 'web-mode-hook 'emmet-mode))

(setq css-indent-level 2)
(setq css-indent-offset 2)

(setq org-startup-folded t)

(setq org-startup-indented t) ;; ident for each level
(setq org-startup-with-inline-images t)

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
