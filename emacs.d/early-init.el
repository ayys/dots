;; -*- lexical-binding: t; -*-
(require 'package)

(setq package-archives nil) ;; nix handles this

;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; sometimes a key to just activate the mark is wanted
(global-set-key "\M-i" (lambda () (interactive) (activate-mark)))
;; resettle the previous occupant
(global-set-key "\M-I" #'tab-to-tab-stop)

(setq mark-even-if-inactive t)

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
;; Improve performance with language servers.
(setq read-process-output-max (* 1024 1024)) ;; 1 MB


;; tree-sitter performance improvement
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)



(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setq default-frame-alist '((fullscreen . maximized)
                            ;; You can turn off scroll bars by uncommenting these lines:
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (ns-transparent-titlebar . t)))


(setq native-comp-speed 3)
(setq native-comp-compiler-options '("-march=znver3" "-Ofast" "-g0" "-fno-finite-math-only" "-fgraphite-identity" "-floop-nest-optimize" "-fdevirtualize-at-ltrans" "-fipa-pta" "-fno-semantic-interposition" "-flto=auto" "-fuse-linker-plugin"))

(setq native-comp-driver-options '("-march=znver3" "-Ofast" "-g0" "-fno-finite-math-only" "-fgraphite-identity" "-floop-nest-optimize" "-fdevirtualize-at-ltrans" "-fipa-pta" "-fno-semantic-interposition" "-flto=auto" "-fuse-linker-plugin"))



(require 'use-package)
(setq use-package-compute-statistics t)

;; disable lsp logging
(lsp-log-io nil)



;; disable line number with large files
(defun disable-line-numbers-if-large-file ()
  "Disable line numbers if the buffer has more than 1000 lines."
  (when (> (count-lines (point-min) (point-max)) 1000)
    (display-line-numbers-mode 0)))

(add-hook 'find-file-hook #'disable-line-numbers-if-large-file)
