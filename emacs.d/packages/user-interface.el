(provide 'user-interface)

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


(use-package nyan-mode :ensure t
  :hook ((fundamental-mode . nyan-mode))
  :config
  (nyan-start-animation))

;; (use-package nerd-icons
;;   :ensure t
  
;;   :hook (after-init . (lambda () (require 'nerd-icons))))

(use-package focus
  :bind (("C-c C-c C-l C-f" . focus-mode))
  :ensure t)

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
