(load-file "prelude.el")

(use-package lsp-python-ms
  :ensure t
  
  :hook ((python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (ayys/py-auto-lsp)))
         (python-ts-mode . (lambda ()
                             (require 'lsp-python-ms)
                             (ayys/py-auto-lsp)))))

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

(use-package dashboard
  :ensure t
  :hook ((dashboard-mode . (lambda () (interactive) (display-line-numbers-mode 0))))
  :init
  (setq dashboard-banner-logo-title "าयउ ऽकषउ")
  (setq dashboard-startup-banner "~/junk/dashboard_pic.png")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-items '((projects . 5)
                          (bookmarks . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config (dashboard-setup-startup-hook))
(use-package magit
  :ensure t
  :config
  (define-key magit-mode-map ";" 'my/magit-new-branch-from-main)
  (setq magit-prefer-remote-upstream t)
  :bind
  (("C-x m" . magit-diff-unstaged)
   ("C-x C-g" . magit-status-quick)
   ("C-x g" . magit-status)))
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode))
(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-delta-args
        '("--24-bit-color" "always"
          "--features" "magit-delta"
          "--color-only")))
(use-package forge
  :ensure t
  :after magit
  :hook ((forge-post-mode . auto-fill-mode)
         (forge-post-mode . turn-on-flyspell)
         (forge-post-mode . my-fetch-all-forge-topics)
         (forge-post-mode . display-fill-column-indicator-mode)))
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
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-clang)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0))
(use-package eat :ensure t
  :bind (("M-RET" . eat))
  :hook (eat-mode . (lambda () (interactive) (display-line-numbers-mode 0))))
(use-package wc-mode
  :ensure t)
(use-package dired
  :ensure f
  :hook (dired-mode . dired-hide-details-mode)
  :ensure nil
  :config
  (setq dired-dwim-target t)
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "\\") (lambda () (interactive) (find-alternate-file "..")))
  (define-key dired-mode-map (kbd ",") 'dired-hide-details-mode)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)) ; “top” means ask once
  :custom
  (dired-listing-switches "-aBhl --group-directories-first"))
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow-regexp)))
(use-package dired-rainbow
  :ensure t
  :config
  (progn
    (dired-rainbow-define-chmod directory "#8AA6BF" "d.*")
    (dired-rainbow-define html "#C47891" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#536A89" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#A478A8" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#AD5B8F" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#7688A2" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#CC6E51" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#C5727F" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#AB8431" ("log"))
    (dired-rainbow-define shell "#E38752" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    ;; Both Light and Dark Themes
    (dired-rainbow-define interpreted "#98B384" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#99B8A0" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#A17FA7" ("exe" "msi"))
    (dired-rainbow-define compressed "#A39984" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#E3B170" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#EDE356" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#8AA6BF" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#C9555B" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#6883A5" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#98B384" "-.*x.*")
    )
  )
(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))
(use-package yaml-pro
  :mode (
         ("\\.yml\\'" . yaml-pro-mode)
         ("\\.yaml\\'" . yaml-pro-mode))
  :ensure t)
(use-package paredit
  :ensure t
  :hook ((scheme-mode  emacs-lisp-mode) . paredit-mode ))
(use-package git-gutter
  :ensure t
  :after magit
  :config
  (add-hook 'prog-mode-hook 'git-gutter-mode))
(use-package rainbow-delimiters
  :ensure t)
(use-package restclient
  :ensure t )
(use-package windmove
  :ensure t
  :bind (("C-c h" . windmove-left)
	 ("C-c l" . windmove-right)
	 ("C-c k" . windmove-up)
	 ("C-c j" . windmove-down)))
(use-package company-posframe :ensure t
  :config (company-posframe-mode t))
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
  (define-key org-mode-map (kbd "C-c C-r") 'verb-command-map)
  (setq org-agenda-files '())
  (setq org-log-done t)
  (add-to-list 'org-latex-packages-alist
               '("AUTO" "polyglossia" t ("xelatex" "lualatex"))))

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

(use-package org-project-capture :ensure t )
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
  :hook (prog-mode . yas-minor-mode)
  :ensure t)
(use-package yasnippet-snippets
  :ensure t)
(use-package flycheck
  :ensure t)
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))
;; (use-package rustic
;;   :ensure t
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   (setq rustic-format-on-save t)
;;   (setq lsp-inlay-hint-enable t)
;;   :hook
;;   (rustic-mode . tree-sitter-hl-mode)
;;   (rustic-mode . lsp)
;;   (rustic-mode . lsp-inlay-hints-mode)
;;   (rustic-mode . lsp-ui-mode)
;;   :custom
;;   (rustic-rustfmt-config-alist '((edition . "2021"))))
(use-package tree-sitter :ensure t
  :hook ((python-ts-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (rust-mode . lsp-inlay-hints-mode)
         (rust-mode . lsp-ui-mode)
         (rust-mode . tree-sitter-hl-mode))
  :config
  (add-to-list 'tree-sitter-major-mode-language-alist '(python-ts-mode . python))
  (add-to-list 'tree-sitter-major-mode-language-alist '(rust-mode . rust)))
(use-package tree-sitter-langs
  :load-path "~/git/tree-sitter-langs"
  :ensure t)

(use-package graphql-mode
  :mode (("\\.gql\\'" . graphql-mode)
         ("\\.graphql\\'" . graphql-mode))
  :ensure t)
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map ("C-q" . vterm-send-next-key))
  :config (setq vterm-shell "bash")
  (global-set-key (kbd "C-z v") 'vterm)
  :hook ((vterm-mode . (lambda () (interactive) (display-line-numbers-mode 0)))))
(use-package doom-themes :ensure t)
(use-package ef-themes :ensure t)
(use-package catppuccin-theme :ensure t)
(use-package circadian
  :ensure t
  :init
  (setq calendar-latitude 27.71)
  (setq calendar-longitude 85.32)
  (setq circadian-themes '((:sunrise . ef-deuteranopia-light)
                           (:sunset . doom-dark+)))
  :config (circadian-setup))
(use-package emacs
  :bind (("C-'" . load-theme)
         ("C-\"" . disable-theme)
         ("C-c C-/" . revert-buffer-no-confirm)
         ("C-:" . goto-line))
  )
(use-package nyx-theme :ensure t)
(use-package hima-theme  :ensure t)
(use-package tao-theme
  :ensure t)
(use-package almost-mono-themes
  :ensure t)
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))
;;; typo-theme.el ends here
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
  :bind (("C-c C-l C-f" . focus-mode))
  :ensure t)
(use-package powerline
  :load-path "~/git/powerline"
  :init (progn
          (setq powerline-display-minor-modes nil)
          (setq powerline-arrow-shape 'arrow14) ;; give your mode-line curves
          (setq powerline-image-apple-rgb t)
          (setq powerline-default-separator-dir '(right . left))
          (setq powerline-default-separator 'wave)
          )
  :config (powerline-default-theme))
(use-package centered-window :ensure t
  :bind (("C-x C-l" . centered-window-mode))
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
(use-package ag
  :ensure t)
(use-package drag-stuff
  :ensure t
  :hook (prog-mode . drag-stuff-mode )
  :bind (("M-p" . drag-stuff-up)
	 ("M-n" . drag-stuff-down)))
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
(use-package highlight-symbol
  :ensure t
  :bind (("C-c C-n" . highlight-symbol-next)
	 ("C-c r" . highlight-symbol-query-replace)
	 ("C-c C-b" . highlight-symbol-prev)))
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c m q" . mc/mark-previous-like-this)
   ("C-c m t" . mc/mark-next-like-this)
   ("C-c m w" . mc/mark-all-symbols-like-this)
   ("C-c m e" . mc/edit-lines)
   ("C-c m r" . mc/mark-all-like-this)))
(use-package repeat :ensure t
  :bind ("C-x z" . repeat)
  :config (repeat-mode t))
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package rg
  :ensure t
  :after drag-stuff)
(use-package consult
  :ensure t
  :bind (         ;; Replacing default keybindings with consult versions
         ("C-s" . consult-line)           ; Line-based search
         ("C-x b" . consult-buffer)       ; Enhanced buffer switcher
         ("C-x v" . consult-vterm-buffer)       ; vterm buffer switcher
         ("C-c b" . consult-project-buffer)       ; Enhanced buffer switcher
         ("C-x r b" . consult-bookmark)   ; Bookmarks
         ("C-c d" . consult-ripgrep)      ; run ripgrep on project
         ("C-c h" . consult-outline)      ; Grep for headings in the file
         )
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (consult-customize :preview-key nil)
  (setq consult-narrow-key "<") ;; "C-+"
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))
(use-package ripgrep :ensure t
  
  :bind (("C-c r" . ripgrep-regexp))
  :config
  (rg-enable-default-bindings))
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the modep gets enabled right away. Note that this forces loading the
  ;; package.
  (require 'marginalia)
  (marginalia-mode))
(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (require 'all-the-icons-completion)
  (all-the-icons-completion-mode))
(use-package vertico
  :ensure t
  :bind
  (("M-C-." . vertico-repeat)
   :map vertico-map
   ("<tab>" . vertico-insert)
   ("<escape>" . minibuffer-keyboard-quit)
   ("?" . minibuffer-completion-help)
   ("C-M-n" . vertico-next-group)
   ("C-M-p" . vertico-previous-group)
   ("<backspace>" . vertico-directory-delete-char)
   ("C-w" . vertico-directory-delete-word)
   ("C-<backspace>" . vertico-directory-delete-word)
   ("RET" . vertico-directory-enter)
   ("C-i" . vertico-quick-insert)
   ("C-o" . vertico-quick-exit)
   ("M-o" . kb/vertico-quick-embark)  ; Ensure kb/vertico-quick-embark is defined
   ("M-G" . vertico-multiform-grid)
   ("M-F" . vertico-multiform-flat)
   ("M-R" . vertico-multiform-reverse)
   ("M-U" . vertico-multiform-unobtrusive)
   ("C-l" . kb/vertico-multiform-flat-toggle))  ; Ensure kb/vertico-multiform-flat-toggle is defined
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :custom
  (vertico-count 5)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 20)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  :init
  (require 'vertico)
  (require 'vertico-multiform)
  (require 'vertico-repeat)
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  )
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))
;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal          ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.
If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))
  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))
  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))
  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )
(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-delay 0.0)
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  )
(use-package noman
  :load-path "~/git/noman.el")
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
(use-package wrap-region
  :ensure t
  
  :hook (prog-mode . wrap-region-mode)
  :config
  (wrap-region-global-mode t))
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))
(use-package smartparens :ensure t
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil))
(use-package avy :ensure t
  :bind (("C-z p" . avy-goto-char-timer)))
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))
(use-package valign :ensure t
  :hook ((org-mode . valign-mode)
         (markdown-mode . valign-mode))
  :config (setq valign-fancy-bar nil) ;; non-nil for fancy unicode bar
  )
(use-package devil :ensure
  :bind (("C-," . global-devil-mode))
  :init (global-devil-mode))
(use-package separedit :ensure t
  :config (setq separedit-default-mode 'markdown-mode)
  
  :bind ("C-c '" . 'separedit))
(use-package lua-mode :ensure t)
(use-package goto-last-change)
(use-package kele
  :config
  (kele-mode 1)
  (bind-key (kbd "M-C-k") kele-command-map kele-mode-map))
(use-package cyberpunk-theme)
(use-package direnv
  :config
  (direnv-mode))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :bind (("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion))
  :hook (prog-mode . copilot-mode))
(use-package string-inflection
  :ensure t
  :bind ("C-c C-u" . string-inflection-all-cycle))
(use-package elfeed
  :ensure t
  :defer t
  :commands (elfeed)
  :custom
  (url-queue-timeout 30)
  (elfeed-feeds
   '(("https://mazzo.li/rss.xml" c low-level unix)
     ("https://simblob.blogspot.com/feeds/posts/default" gamedev math algorithms)
     ("https://box2d.org/posts/index.xml" gamedev math algorithms)
     "https://davidgomes.com/rss/"
     ("https://fabiensanglard.net/rss.xml" retrogaming)
     ("https://ferd.ca/feed.rss" distsys)
     "https://blog.singleton.io/index.xml"
     ("https://johnnysswlab.com/feed/" cpp performance)
     ("https://jvns.ca/atom.xml" webdev)
     ("https://matklad.github.io/feed.xml" low-level programming)
     ("https://jonathan-frere.com/index.xml" programming)
     ("https://notes.eatonphil.com/rss.xml" distsys programming)
     ("https://samwho.dev/blog" programming visualization)
     ("https://wingolog.org/feed/atom" compilers guile scheme)
     ("https://jakelazaroff.com/rss.xml" webdev)
     ("https://www.localfirstnews.com/rss/" local-first)
     ("https://www.internalpointers.com/rss" networking concurrency)
     ("https://hazelweakly.me/rss.xml" observability)
     ("https://norvig.com/rss-feed.xml" software)
     ("https://hnrss.org/frontpage" hackernews)
     ("https://blog.rust-lang.org/feed.xml" rust)
     ("https://turreta.com/feed/" rust)
     ("http://blog.japaric.io/index.xml" rust)
     ("https://pypi.org/rss/project/django/releases.xml" python django release pypi)
     ("https://pythonspeed.com/atom.xml" python))))
(use-package gited)
(use-package key-chord
  :config (key-chord-define-global ";;" "\C-e;"))
(use-package bind-chord)
