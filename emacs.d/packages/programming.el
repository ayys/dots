(provide 'programming)

(use-package nix-mode :ensure t)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))



(use-package lsp-mode :ensure t
  :hook ((tsx-ts-mode . lsp-deferred))
  )

(defun ayys/py-workon-project-venv ()
  "Activate the first .venv virtual environment found in the Projectile project."
  (let ((venv-path (directory-files-recursively (projectile-project-root) "^\.venv$" :include-directories t)))
    (if (and venv-path (file-directory-p (car venv-path)))
        (progn
          (pyvenv-activate (car venv-path))
          (car venv-path))
      (pyvenv-deactivate)
      nil)))

(defun ayys/py-auto-lsp ()
  "Turn on lsp mode in a Python project with some automated logic.
Try to automatically determine which pyenv virtual environment to
activate based on the project name, using
`ayys/py-workon-project-venv'. If successful, call `lsp'. If we
cannot determine the virtualenv automatically, first call the
interactive `pyvenv-activate' function before `lsp'"
  (interactive)

  (let ((pvenv (if (and (boundp 'pyvenv-virtual-env-name) pyvenv-virtual-env-name)
                   pyvenv-virtual-env-name
                 (ayys/py-workon-project-venv))))
    (if pvenv
        (lsp)
      (progn
        (call-interactively #'pyvenv-activate)
        (lsp)))))

(use-package lsp-python-ms
  :ensure t
  
  :hook ((python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (ayys/py-auto-lsp)))
         (python-ts-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (ayys/py-auto-lsp)))))
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (("M-." . lsp-ui-peek-find-definitions)
         ("M-?" . lsp-ui-peek-find-references))
  )

(use-package ruff-format :ensure t

  :hook (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode))

(use-package web-mode :defer t :ensure t
  :mode (
("\\.html?\\'" . web-mode)
("\\.mustache\\'" . web-mode)
("\\.phtml\\'" . web-mode)
("\\.tpl\\.php\\'" . web-mode)
("\\.erb\\'" . web-mode)
("\\.djhtml\\'" . web-mode)
("\\.as[cp]x\\'" . web-mode)
("\\.[agj]sp\\'" . web-mode)
("\\.php\\'" . web-mode)
("\\.twig\\'" . web-mode)
("\\.jsp\\'" . web-mode)
("\\.jspf\\'" . web-mode)
("\\.tag\\'" . web-mode)))

(use-package editorconfig
  :ensure t)

(use-package sqlformat
  :ensure t
  :hook ((sql-mode . sqlformat-on-save-mode))
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("--format-type")))

(use-package pyvenv
  :ensure t)


(use-package yasnippet
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  (setq lsp-inlay-hint-enable t)
  :hook
  (rustic-mode . tree-sitter-hl-mode)
  (rustic-mode . lsp)
  (rustic-mode . lsp-inlay-hints-mode)
  (rustic-mode . lsp-ui-mode)
  :custom
  (rustic-rustfmt-config-alist '((edition . "2021"))))

(use-package tree-sitter :ensure t
  :hook ( (python-ts-mode . lsp-deferred)
          (rust-ts-mode . lsp-inlay-hints-mode)
          (rust-ts-mode . lsp-ui-mode)
          (rust-ts-mode . lsp-deferred))
  :config (add-to-list 'tree-sitter-major-mode-language-alist '(python-ts-mode . python)))

(use-package tree-sitter-langs
  :load-path "~/git/tree-sitter-langs"
  :ensure t)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; open tsx files with typescript mode
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))


(use-package graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode))
  :ensure t)


(use-package lua-mode :ensure t)
