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
