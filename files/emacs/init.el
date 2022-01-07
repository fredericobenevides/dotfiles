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
         ("<f2> u" . counsel-unicode-char))
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

(setq org-startup-folded t)

(setq org-startup-indented t) ;; ident for each level
(setq org-startup-with-inline-images t)

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
