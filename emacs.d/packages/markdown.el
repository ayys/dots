(use-package org
  :ensure t
  
  :bind
  (("C-c a" . org-agenda)
   ("C-c C-h" . org-html-export-to-html)
   ("C-c l" . org-store-link))
  :hook ((org-mode . auto-fill-mode)
         (org-mode . display-fill-column-indicator-mode)
         )
  :config
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "babel" t ("pdflatex")))
  (define-key org-mode-map (kbd "`") #'org-insert-tilde)
  (define-key org-mode-map (kbd "~") #'org-insert-backtick)
  (define-key org-mode-map (kbd "C-c C-r") 'verb-command-map)
  (setq org-agenda-files '())
  (setq org-log-done t)
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex"))))


(setq org-confirm-babel-evaluate nil)

(use-package org-bullets
  :ensure t
  
  :hook (org-mode . org-bullets-mode))
(use-package org-appear
  :ensure t
  
  :hook (org-mode . org-appear-mode))
(use-package org-superstar
  :ensure t
  
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))
(use-package org-rainbow-tags :ensure t
  
  :hook ((org-mode . org-rainbow-tags-mode)))

(use-package markdown-mode
  
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((org-mode . auto-fill-mode)
         (markdown-mode . auto-fill-mode)
         (org-mode . display-fill-column-indicator-mode)
         (markdown-mode . display-fill-column-indicator-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t))


(defun ayys/org-projectile-todo-files ()
  "Fetch a list of org TODO files for projects that actually exist."
  (require 'org-projectile)
  (seq-filter #'file-exists-p (occ-get-todo-files org-project-capture-strategy)))

(defun ayys/org-projectile-goto-project-file ()
  "Open the TODO.org file for the current project."
  (interactive)
  (org-projectile-goto-location-for-project (projectile-project-name)))


(defun ayys/org-projectile:update-agenda-files ()
  "Add all `org-projectile' files to `org-agenda-files'."
  (interactive)
  (dolist (file (ayys/org-projectile-todo-files))
    (add-to-list 'org-agenda-files file)))

(use-package org-project-capture :ensure t )

(provide 'markdown)
