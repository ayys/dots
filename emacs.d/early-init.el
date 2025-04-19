(require 'package)

;; (setq package-archives nil) ;; nix handles this

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; sometimes a key to just activate the mark is wanted
(global-set-key "\M-i" (lambda () (interactive) (activate-mark)))
;; resettle the previous occupant
(global-set-key "\M-I" #'tab-to-tab-stop)

(setq mark-even-if-inactive t)

