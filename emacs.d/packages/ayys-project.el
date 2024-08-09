(provide 'ayys-project)

(defun open-project-readme ()
  "Open the README.md file in the project root, ignoring case."
  (let ((root (projectile-project-root)))
    (when root
      (let ((readme (car (directory-files root t "\\`README\\.md\\'" t))))
        (when readme
          (find-file readme))))))

(use-package projectile
  :ensure t
  :after vertico
  :init
  (projectile-mode +1)
  :bind ((:map projectile-mode-map
               ("s-p" . projectile-command-map)
               ("C-c p" . projectile-command-map))
         (:map projectile-command-map
               ("RET" . projectile-run-async-shell-command-in-root)
               ("!" . projectile-run-shell-command-in-root)
               ("-" . projectile-run-async-shell-command-in-root)
               ("r" . open-project-readme)))
  :config (progn
    (setq projectile-sort-order 'recently-active)
    (setq projectile-indexing-method 'hybrid)
    (define-key projectile-mode-map (kbd "C-c -") 'projectile-run-async-shell-command-in-root)
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action #'magit-status)
    (setq projectile-completion-system 'default)
    (setq projectile-project-search-path '("~/git"))))
