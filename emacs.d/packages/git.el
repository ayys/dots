(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-delta-args
  '("--24-bit-color" "always"
    "--features" "magit-delta"
    "--color-only")))

(defun string-to-branch-name (str)
  "Turns a string into a branch name by replacing spaces with dashes and lowercasing it."
  ;; remove trailing whitespace
  (setq str (replace-regexp-in-string "\\(\\s-+\\)$" "" str))
  ;; remove all non-word characters except dashes, underscores, and slashes
  (setq str (replace-regexp-in-string "[^a-zA-Z0-9-_ /]" "" str))
  ;; remove trailing dashes
  (setq str (replace-regexp-in-string "\\(-+\\)$" "" str))
  ;; remove double dashes
  (setq str (replace-regexp-in-string "\\(-+\\)" "-" str))
  (downcase (replace-regexp-in-string " " "-" str)))

(defun my/magit-new-branch-from-main ()
  "Create a new branch from main."
  (interactive)
  (magit-branch-and-checkout (string-to-branch-name (read-string "Branch name: ")) "main"))

(use-package magit
  :ensure t
  :config
  (define-key magit-mode-map ";" 'my/magit-new-branch-from-main)
  (setq magit-prefer-remote-upstream t)
  :bind
  (("C-x m" . magit-diff-unstaged)
   ("C-x C-g" . magit-status-quick)
   ("C-x g" . magit-status)))

(defun my-fetch-all-forge-topics ()
  "Fetch all topics from the forge remote."
  (when (and (derived-mode-p 'forge-topic-mode)
             ;; Add any additional conditions to verify topic creation
             )
    (forge-pull)))


(use-package forge
  :ensure t
  :after magit
  :hook ((forge-post-mode . auto-fill-mode)
         (forge-post-mode . turn-on-flyspell)
         (forge-post-mode . my-fetch-all-forge-topics)
         (forge-post-mode . display-fill-column-indicator-mode)))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t )

(use-package blamer
  :ensure t
  :bind (("C-c i" . blamer-show-commit-info))
  :custom
  (blamer-idle-time 1)
  (blamer-min-offset 50)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :height 0.7
                   :italic t))))


(use-package git-link
  :ensure t)
