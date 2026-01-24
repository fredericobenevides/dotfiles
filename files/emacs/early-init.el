;; Prevent the native package manager (package.el) from loading at startup.
;; This is mandatory when using straight.el to avoid package conflicts.
(setq package-enable-at-startup nil)
(setq package--init-file-ensured t)

;; Optimize Garbage Collection during startup for better performance.
;; Setting this to the maximum value prevents GC from running while loading files.
(setq gc-cons-threshold most-positive-fixnum)

;; Environment variables for LSP performance.
(setenv "LSP_USE_PLISTS" "true")