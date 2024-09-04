;; bootstrap with straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; avoid issues with different verion from org
(straight-use-package '(org :type built-in))

;;  Effectively replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(setq-default
 auto-save-default nil
 create-lockfiles nil
 column-number-mode t
 fill-column 80
 indent-tabs-mode nil
 make-backup-files nil)

;; broken automatically when the line becomes too wide
(auto-fill-mode)

;; show line numbers
(global-display-line-numbers-mode)

;; pair mode
(electric-pair-mode)

;; enables highlighting for the current buffer
(when window-system (global-hl-line-mode t))

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 140)
(set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 140)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq inhibit-startup-message t) ;; Don't show the startup message

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; maximize window

(menu-bar-mode -1)    ;; Disable menu bar
(scroll-bar-mode -1)  ;; Disable visible scrollbar
(tool-bar-mode -1)    ;; Disable the toolbar
(tooltip-mode -1)     ;; Disable tooltips

(set-fringe-mode 10)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;;(setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.=
  :init
  (global-corfu-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)              ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-flycheck)

(use-package marginalia
  :after
  (nerd-icons-completion)

  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; This will ensure that it is on when marginalia-mode is on and is off when it’s off.
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-w C-S-w") 'mc/mark-all-dwim)
  (global-set-key (kbd "C-S-e C-S-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

(show-paren-mode 1)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package neotree
  :config
  (global-set-key (kbd "<f8>") 'neotree-toggle)
  (global-set-key (kbd "M-<f8>") 'neotree-refresh))

(use-package dashboard
  :straight
  (:host github :repo "emacs-dashboard/emacs-dashboard" :commit "a1c29c0bbfca3f6778022628b79e7eef2b9f351d")
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

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content nil)

  ;; vertically center content
  (setq dashboard-vertically-center-content nil)

  ;; start dashboard
  (dashboard-setup-startup-hook))

(use-package beacon
  :config (beacon-mode))

(use-package command-log-mode
 :config
 (global-command-log-mode)
 :bind ("C-c o" . clm/toggle-command-log-buffer))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package impatient-mode
  :commands impatient-mode)

(use-package simple-httpd
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

(use-package lispy)

(use-package emmet-mode
  :after
  (web-mode css-mode))

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

  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  ;; integrated with emmet
  (add-hook 'web-mode-hook 'emmet-mode))

(setq css-indent-level 2)
(setq css-indent-offset 2)

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  ;;:config
  ;;(add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)
  )

(use-package prettier
  :config
  (add-hook 'css-mode-hook 'prettier-mode)
  (add-hook 'js-mode-hook 'prettier-mode)
  (add-hook 'json-mode-hook 'prettier-mode)
  (add-hook 'web-mode-hook 'prettier-mode))

(use-package json-mode)
;;(use-package js2-mode)
(use-package typescript-mode)

(setq js-indent-level 2)

(use-package clojure-mode)

(use-package cider
  :config
  ;; disable cider showing eldoc during symbol at point
  (setq cider-eldoc-display-for-symbol-at-point nil)

  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)

  ;; When there's a cider error, don't switch to the buffer
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer nil))

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package clj-refactor
  :config
  (setq cljr-warn-on-eval nil) ; disable warning
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "pandoc"))

(defun fb/markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun fb/markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'fb/markdown-filter)
  (imp-visit-buffer))

(defun fb/markdown-preview-stop ()
  "Stop preview"
  (interactive)
  (unless (process-status "httpd")
    (httpd-stop))
  (impatient-mode -1))

(defun my-cargo-run ()
  "Build and run Rust code."
  (interactive)
  (cargo-process-run)
  (let ((orig-win (selected-window))
        (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible)))
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (end-of-buffer)))

(use-package rust-mode
  :config
  (setq rust-format-on-save t)

  ;;(add-hook 'rust-mode-hook (lambda () (prettify-symbols-mode)))

  (define-key rust-mode-map (kbd "C-c C-c C-q") 'my-cargo-run))

(use-package cargo
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package flycheck-rust
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package nerd-icons
  :straight
  (nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el")
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

  (use-package nerd-icons-completion
    :after
    (nerd-icons)
    :init
    (nerd-icons-completion-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package magit)

;;(use-package  magit-delta
;;  :after (magit)
;;  :hook (magit-mode . magit-delta-mode))

(winner-mode 1)

(package-install 'org)

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-confirm-babel-evaluate nil
 org-special-ctrl-a/e t
 org-startup-indented t
 org-startup-folded t
 ;;      org-startup-with-inline-images t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")

(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (setq org-roam-directory (file-truename "~/.dotfiles/external_files/org"))
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui)
