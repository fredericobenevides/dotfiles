;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Frederico Benevides"
      user-mail-address "contato@fredericobenevides.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrainsMono NerdFont" :size 16)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono NerdFont" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; set the localleader key
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; Maximize window
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Disable undo-history after closing emacs
(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

;; Change splash screen. Images download from
;; https://gitlab.com/zzamboni/dot-doom/-/tree/master/splash
(let ((alternatives '("doom-emacs-flugo-slant_out_purple-small.png")))
  (setq fancy-splash-image
        (concat doom-private-dir "splash/"
                (nth (random (length alternatives)) alternatives))))

(use-package! cider
  :after clojure-mode
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil) ; use lsp to show eldoc during symbol at point
  (set-lookup-handlers! 'cider-mode nil) ; use lsp to find definition/references
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point)))) ; use lsp completion

(use-package! clj-refactor
  :after clojure-mode
  :config
  (setq cljr-add-ns-to-blank-clj-files nil)) ; use lsp to add ns to blank files

(use-package! lsp-mode
  :config
  (setq lsp-clojure-custom-server-command '("bash" "-c" "clojure-lsp")
        lsp-completion-show-details t
        lsp-eldoc-enable-hover t
        lsp-lens-enable t) ;; shows the references
  )

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60))

(use-package smartparens
  :after clojure-mode
  :bind
  (:map smartparens-mode-map
        ("C-M-<up>"      . sp-raise-sexp)
        ("C-<right>"     . sp-forward-slurp-sexp)
        ("C-<left>"      . sp-backward-slurp-sexp)
        ("M-<right>"     . sp-forward-barf-sexp)
        ("M-<left>"      . sp-backward-barf-sexp)
        ("C-k"           . sp-kill-hybrid-sexp)
        ("C-x C-t"       . sp-transpose-hybrid-sexp)
        ("C-M-n"         . sp-next-sexp)
        ("C-M-p"         . sp-previous-sexp)
        ("C-<backspace>" . sp-backward-kill-word)))
