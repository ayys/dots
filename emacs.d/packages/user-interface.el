(use-package all-the-icons
  :ensure t)

(use-package better-defaults
  :hook (after-init . (lambda () (require 'better-defaults)))
  :ensure t )



(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :ensure t
  :config
  (progn
    (setq ibuffer-default-sorting-mode 'major-mode)
    (setq ibuffer-expert t)
    (use-package ibuffer-vc
      :ensure t
      :config
      (progn
        (defun modi/ibuffer-customization ()
          (ibuffer-vc-set-filter-groups-by-vc-root)
          (unless (eq ibuffer-sorting-mode 'alphabetic)
            (ibuffer-do-sort-by-alphabetic)
            (ibuffer-do-sort-by-major-mode)))))
    (add-hook 'ibuffer-hook #'modi/ibuffer-customization)))

(use-package disable-mouse
  :ensure t
  :hook (vterm-mode . disable-mouse-mode))

(use-package nyan-mode :ensure t
  :hook ((fundamental-mode . nyan-mode))
  :config
  (nyan-start-animation))

(use-package nerd-icons
  :ensure t
  :defer t
  :hook (after-init . (lambda () (require 'nerd-icons))))

(use-package focus
  :bind (("C-c C-c C-l C-f" . focus-mode))
  :ensure t)

(use-package eyebrowse
  :ensure t
  :init
  (progn
    (setq eyebrowse-new-workspace t)
    (setq eyebrowse-wrap-around t)
    (setq eyebrowse-mode-line-left-delimiter " ")
    (setq eyebrowse-mode-line-right-delimiter " ")
    (setq eyebrowse-mode-line-separator " "))
  :config (eyebrowse-mode t)
  :bind (("C-z 1" . eyebrowse-switch-to-window-config-1)
         ("C-z 2" . eyebrowse-switch-to-window-config-2)
         ("C-z 3" . eyebrowse-switch-to-window-config-3)
         ("C-z 4" . eyebrowse-switch-to-window-config-4)
         ("C-z 5" . eyebrowse-switch-to-window-config-5)
         ("C-z 6" . eyebrowse-switch-to-window-config-6)
         ("C-z 7" . eyebrowse-switch-to-window-config-7)
         ("C-z 8" . eyebrowse-switch-to-window-config-8)
         ("C-z 9" . eyebrowse-switch-to-window-config-9)
         ("C-z 0" . eyebrowse-switch-to-window-config-0)
         ("C-c C-z 1" . eyebrowse-switch-to-window-config-1)
         ("C-c C-z 2" . eyebrowse-switch-to-window-config-2)
         ("C-c C-z 3" . eyebrowse-switch-to-window-config-3)
         ("C-c C-z 4" . eyebrowse-switch-to-window-config-4)
         ("C-c C-z 5" . eyebrowse-switch-to-window-config-5)
         ("C-c C-z 6" . eyebrowse-switch-to-window-config-6)
         ("C-c C-z 7" . eyebrowse-switch-to-window-config-7)
         ("C-c C-z 8" . eyebrowse-switch-to-window-config-8)
         ("C-c C-z 9" . eyebrowse-switch-to-window-config-9)
         ("C-c C-z 0" . eyebrowse-switch-to-window-config-0)
         ("C-z =" . eyebrowse-rename-window-config)
         ("C-z +" . eyebrowse-last-window-config)
         ("C-z _" . eyebrowse-close-window-config)
         ("C-z `" . eyebrowse-switch-to-window-config)
         ("C-c C-z =" . eyebrowse-rename-window-config)
         ("C-c C-z +" . eyebrowse-last-window-config)
         ("C-c C-z _" . eyebrowse-close-window-config)
         ("C-c C-z `" . eyebrowse-switch-to-window-config)))

;; (use-package perspective
;;   :ensure t
;;   :init
;;   (progn
;;     (setq persp-nil-name "default")
;;     (setq persp-show-modestring nil)
;;     (setq persp-sort 'created))
;;   :config (persp-mode))

(use-package powerline
  :load-path "~/git/powerline"
  :init (progn
          (setq powerline-arrow-shape 'arrow14) ;; give your mode-line curves
          (setq powerline-image-apple-rgb t)
          (setq powerline-default-separator-dir '(right . left))
          (setq powerline-default-separator 'wave)
          )
  :config (powerline-default-theme))

(use-package centered-window :ensure t
  :bind (("C-c C-c C-l" . centered-window-mode))
  :config
  (setq cwm-use-vertical-padding nil)
  (setq cwm-frame-internal-border 0))

(use-package diminish
  :ensure t
  :config
  (progn
    (diminish 'abbrev-mode "")
    (diminish 'auto-revert-mode "")
    (diminish 'company-mode "")
    (diminish 'copile-mode "")
    (diminish 'counsel-mode "")
    (diminish 'drag-stuff-mode "")
    (diminish 'eldoc-mode "")
    (diminish 'fixmee-mode "")
    (diminish 'flymake-mode "")
    (diminish 'flyspell-mode "")
    (diminish 'global-whitespace-mode "")
    (diminish 'helm-mode "")
    (diminish 'ivy-mode "")
    (diminish 'lsp-mode "")
    (diminish 'lsp-lens-mode "")
    (diminish 'projectile-mode "")
    (diminish 'python-mode "")
    (diminish 'python-ts-mode "")
    (diminish 'rainbow-mode "")
    (diminish 'ruff-format-on-save-mode "")
    (diminish 'rustic-mode "")
    (diminish 'tree-sitter-mode "")
    (diminish 'undo-tree-mode "")
    (diminish 'visual-line-mode "")
    (diminish 'whitespace-mode "")
    (diminish 'yas-minor-mode "")
    ))


(use-package cursory
  :ensure t
  :config
  (setq cursory-latest-state-file (locate-user-emacs-file "cursory-latest-state"))
   (cursory-set-preset (or (cursory-restore-latest-preset) 'bar))
   (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)
   (define-key global-map (kbd "C-c C-c p") #'cursory-set-preset))
