;; -*- lexical-binding: t; -*-
(load-file (expand-file-name "./prelude.el" user-emacs-directory))
(load-file (expand-file-name "packages/sxhkd-mode.el" user-emacs-directory))
(use-package avy :ensure t
  :bind (("C-z C-z" . avy-goto-char-timer)
          ("C-z C-l" . avy-goto-line)
          ("C-z C-s" . avy-isearch)
          ("C-z C-w" . avy-goto-word-1)))
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))
(use-package dashboard
  :ensure t
  :hook ((dashboard-mode . (lambda () (interactive) (display-line-numbers-mode 0))))
  :init
  (setq dashboard-banner-logo-title "าयउ ऽकषउ")
  (setq dashboard-startup-banner "~/pics/emacs_dashboard_pic.png")
  (setq dashboard-set-heading-icons t)
  (setq dashboard-items '((projects . 5)
                           (bookmarks . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config (dashboard-setup-startup-hook))
(use-package wc-mode
  :ensure t)
(use-package doom-themes :ensure t)
(use-package ef-themes :ensure t)
(use-package cyberpunk-theme :ensure t)
(use-package catppuccin-theme :ensure t)
(use-package nyx-theme :ensure t)
(use-package hima-theme  :ensure t)
(use-package tao-theme
  :ensure t)
(use-package almost-mono-themes
  :ensure t)
(use-package better-defaults
  :hook (after-init . (lambda () (require 'better-defaults)))
  :ensure t )
(use-package circadian
  :ensure t
  :init
  (setq calendar-latitude 27.71)
  (setq calendar-longitude 85.32)
  (setq circadian-themes '((:sunrise . ef-maris-light)
                            (:sunset . doom-gruvbox)))
  :config (circadian-setup))
(use-package nyan-mode :ensure t
  :hook ((prog-mode . nyan-mode))
  :config
  (nyan-start-animation))
(use-package powerline
  :load-path (expand-file-name "./packages/powerline" user-emacs-directory)
  :init (progn
          (setq powerline-display-minor-modes nil)
          (setq powerline-arrow-shape 'arrow14)
          (setq powerline-image-apple-rgb t)
          (setq powerline-default-separator-dir '(right . left))
          (setq powerline-default-separator 'wave))
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
    (diminish 'counsel-mode "")
    (diminish 'drag-stuff-mode "")
    (diminish 'eldoc-mode "")
    (diminish 'fixmee-mode "")
    (diminish 'flymake-mode "")
    (diminish 'flyspell-mode "")
    (diminish 'global-whitespace-mode "")
    (diminish 'helm-mode "")
    (diminish 'ivy-mode "")
    (diminish 'projectile-mode "")
    (diminish 'python-mode "")
    (diminish 'git-gutter-mode "")
    (diminish 'paredit-mode "")
    (diminish 'company-posframe-mode "")
    (diminish 'editorconfig-mode "")
    (diminish 'python-ts-mode "")
    (diminish 'rainbow-mode "")
    (diminish 'ruff-format-on-save-mode "")
    (diminish 'rustic-mode "🦀ic")
    (diminish 'rust-ts-mode "🦀ts")
    (diminish 'rust-mode "🦀")
    (diminish 'guru-mode "")
    (diminish 'tree-sitter-mode "")
    (diminish 'undo-tree-mode "")
    (diminish 'visual-line-mode "")
    (diminish 'whitespace-mode "")
    (diminish 'yas-minor-mode "")))
(use-package windmove
  :ensure t
  :bind (( "C-z h" . windmove-left)
          ("C-z l" . windmove-right)
          ("C-z k" . windmove-up)
          ("C-z j" . windmove-down)))
(use-package consult
  :ensure t
  :bind (
          ("C-s" . consult-line)           ; Line-based search
          ("C-x b" . consult-buffer)       ; Enhanced buffer switcher
          ("C-x v" . consult-vterm-buffer)       ; vterm buffer switcher
          ("C-c b" . consult-project-buffer)       ; Enhanced buffer switcher
          ("C-x r b" . consult-bookmark)   ; Bookmarks
          ("C-c d" . consult-ripgrep)      ; run ripgrep on project
          ("C-c h" . consult-outline)      ; Grep for headings in the file
          ("C-x <SPC>" . consult-global-mark)      ; Grep for headings in the file
          ("C-c f" . consult-fd)      ; Grep for headings in the file
          )
  :config
  (setq xref-show-xrefs-function #'consult-xref
    xref-show-definitions-function #'consult-xref)
  (consult-customize :preview-key nil)
  (setq consult-narrow-key "<")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))
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
    ("M-B" . vertico-multiform-buffer)
    ("M-F" . vertico-multiform-flat)
    ("M-R" . vertico-multiform-reverse)
    ("M-U" . vertico-multiform-unobtrusive)
    )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
          (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
          )
  :custom
  (vertico-count 5)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 20)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  :init
  (require 'vertico)
  (require 'vertico-multiform)
  (require 'vertico-repeat)
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))
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
  (vertico-multiform-mode)
  (advice-add #'vertico--format-candidate :around
    (lambda (orig cand prefix suffix index _start)
      (setq cand (funcall orig cand prefix suffix index _start))
      (concat
        (if (= vertico--index index)
          (propertize "» " 'face 'vertico-current)
          "  ")
        cand)))
  )
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
          ("M-A" . marginalia-cycle))
  :init
  (require 'marginalia)
  (marginalia-mode))
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
      `(orderless-flex . ,(substring pattern 0 -1)))))
(use-package rg
  :ensure t
  :after drag-stuff)
(use-package ripgrep :ensure t
  :bind (("C-c r" . ripgrep-regexp))
  :config
  (rg-enable-default-bindings))
(use-package savehist
  :ensure t
  :init
  (savehist-mode))
(use-package separedit :ensure t
  :config (setq separedit-default-mode 'markdown-ts-mode)
  :bind ("C-c '" . 'separedit))
(use-package goto-last-change)
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
  (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo")))
  (setq undo-tree-visualizer-relative-timestamps t))
(use-package smartparens :ensure t
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'scheme-mode "'" nil :actions nil)
  (sp-local-pair 'scheme-mode "`" nil :actions nil))
(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq hl-paren-delay 0.0)
  (setq hl-paren-colors '("Springgreen3"
                           "IndianRed1"
                           "IndianRed3"
                           "IndianRed4"))
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))
(use-package drag-stuff
  :ensure t
  :hook (prog-mode . drag-stuff-mode )
  :bind (("M-p" . drag-stuff-up)
          ("M-n" . drag-stuff-down)))
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
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
            (setq projectile-indexing-method 'alien)
            (define-key projectile-mode-map (kbd "C-c -") 'projectile-run-async-shell-command-in-root)
            (setq projectile-enable-caching t)
            (setq projectile-switch-project-action #'magit-status)
            (setq projectile-completion-system 'default)
            (setq projectile-project-search-path '("~/git"))))
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-clang)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-idle-delay 0.1))
(use-package company-posframe :ensure t
  :config (company-posframe-mode t))
(use-package flycheck
  :ensure t)
(use-package paredit
  :ensure t
  :hook ((scheme-mode  emacs-lisp-mode) . paredit-mode ))
(use-package rainbow-delimiters
  :ensure t)
(use-package restclient
  :ensure t )
(use-package sqlformat
  :ensure t
  :hook ((sql-mode . sqlformat-on-save-mode))
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("--format-type")))
(use-package pyvenv
  :ensure t)
(use-package nix-mode :ensure t)
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
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package lua-mode :ensure t)
(use-package graphql-mode
  :ensure t
  :mode ("\\.graphql\\'" "\\.gql\\'"))
(use-package yaml-pro
  :mode (
          ("\\.yml\\'" . yaml-ts-mode)
          ("\\.yaml\\'" . yaml-ts-mode))
  :ensure t)
(use-package direnv
  :config
  (direnv-mode)
  :custom (setq direnv-always-show-summary nil))
(use-package magit
  :ensure t
  :config
  (define-key magit-mode-map ";" 'my/magit-new-branch-from-main)
  (setq magit-clone-default-directory "~/git/")
  (setq magit-prefer-remote-upstream t)
  (setq magit-refresh-status-buffer nil)
  :bind
  (("C-x m" . magit-diff-unstaged)
    ("C-x C-g" . magit-status-quick)
    ("C-x g" . magit-status)))
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode))
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
  :config
  (progn
    (add-to-list 'git-link-remote-alist '("mvv" git-link-github) t)
    (add-to-list 'git-link-homepage-remote-alist '("mvv" git-link-homepage-github) t)
    (add-to-list 'git-link-commit-remote-alist '("mvv" git-link-commit-github) t)

    (add-to-list 'git-link-remote-alist '("lyric" git-link-gitlab) t)
    (add-to-list 'git-link-homepage-remote-alist '("lyric" git-link-homepage-github) t)
    (add-to-list 'git-link-commit-remote-alist '("lyric" git-link-commit-gitlab) t))
  :bind (("C-c C-g" . git-link-dispatch))
  :ensure t)
(use-package git-gutter
  :ensure t
  :after magit
  :config
  (add-hook 'prog-mode-hook 'git-gutter-mode))
(use-package gited)
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
;; (use-package org-superstar
;;   :ensure t
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-special-todo-items t))
(use-package org-rainbow-tags :ensure t
  :hook ((org-mode . org-rainbow-tags-mode)))
(use-package markdown-ts-mode
  :mode (("README\\.md\\'" . markdown-ts-mode)
          ("\\.md\\'" . markdown-ts-mode)
          ("\\.mdx\\'" . markdown-ts-mode)
          ("\\.markdown\\'" . markdown-ts-mode))
  :hook ((markdown-ts-mode . auto-fill-mode)
          (markdown-ts-mode . display-fill-column-indicator-mode)))
(use-package org-project-capture
  :bind (("C-c n p" . org-project-capture-project-todo-completing-read))
  :ensure t
  :config
  (progn
    (setq org-project-capture-backend
      (make-instance 'org-project-capture-project-backend))
    (setq org-project-capture-per-project-filepath ".todo/TODO.org")
    (org-project-capture-per-project)))
(use-package eat :ensure t
  :bind (("M-RET" . ayys/consult-project-eat-buffers))
  :bind (:map eat-mode-map ("M-<return>" . ayys/consult-project-eat-buffers))
  :hook (eat-mode . (lambda () (interactive) (display-line-numbers-mode 0))))

(use-package emacs
  :config
  (progn
    (setq auto-window-vscroll nil)
    (setq fast-but-imprecise-scrolling t)
    (setq redisplay-dont-pause t)
    (setq jit-lock-defer-time 0.05)
    (setq redisplay-skip-fontification-on-input t))
  :bind (("C-'" . load-theme)
          ("C-\"" . disable-theme)
          ("M-j" . duplicate-dwim)
          ("C-c C-/" . revert-buffer-no-confirm)
          ("C-:" . goto-line)))
(use-package focus
  :bind (("C-c C-l C-f" . focus-mode))
  :ensure t)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (require 'all-the-icons-completion)
  (all-the-icons-completion-mode))
(use-package noman
  :load-path (expand-file-name "./packages/noman.el/" user-emacs-directory))
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
(use-package valign :ensure t
  :hook ((org-mode . valign-mode)
          (markdown-ts-mode . valign-mode))
  :config (setq valign-fancy-bar nil)
  )
(use-package string-inflection
  :ensure t
  :bind ("C-c C-u" . string-inflection-all-cycle))
(use-package webjump
  :bind (("C-c C-o" . webjump)))
(use-package rfc-mode)
(use-package ido
  :config (ido-mode -1))

(use-package ibuffer-projectile
  :ensure t
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
      ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
      ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
      ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
      (t (format "%8dB" (buffer-size)))))
  (setq ibuffer-formats
    '((mark modified read-only " "
        (name 25 25 :left :elide)
        " "
        (size-h 9 -1 :right)
        " "
        (mode 16 16 :left :elide)
        " "
        project-relative-file)))
  )
(use-package guru-mode
  :hook ((prog-mode . guru-mode)))
(use-package terraform-mode)
(use-package kubed
  :bind ("C-c k" . kubed-transient))
(use-package sxhkd-mode :ensure t)
(use-package solaire-mode
  :ensure t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (solaire-global-mode +1))
(use-package wat-ts-mode :ensure t)
(use-package org-modern)
(use-package geiser-guile)
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
    ("C-c n r" . denote-rename-file)
    ("C-c n l" . denote-link)
    ("C-c n b" . denote-backlinks)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/docs/notes/"))
  (denote-rename-buffer-mode 1))
(use-package debbugs)
(use-package debpaste)
(use-package erc
  :config
  (setopt erc-modules
    (seq-union '(sasl nicks bufbar nickbar scrolltobottom)
      erc-modules))
  :custom
  (erc-inhibit-multiline-input t)
  (erc-send-whitespace-lines t)
  (erc-ask-about-multiline-input t)
  (erc-scrolltobottom-all t)
  (erc-server-reconnect-function #'erc-server-delayed-check-reconnect)
  (erc-server-reconnect-timeout 30)
  (erc-interactive-display 'buffer)
  :bind (:map erc-mode-map
          ("RET" . nil)
          ("C-<return>" . #'erc-send-current-line))
  :custom-face (erc-notice-face ((t (:slant italic :weight unspecified)))))
(use-package erc-sasl
  :custom (erc-sasl-user :nick))
(use-package erc-join
  :custom (erc-autojoin-channels-alist '((Libera.Chat "#guix"))))
(use-package erc-fill
  :custom
  (erc-fill-function #'erc-fill-wrap)
  (erc-fill-static-center 18)
  :bind (:map erc-fill-wrap-mode-map ("C-c =" . #'erc-fill-wrap-nudge)))
(use-package erc-track
  :config (setopt erc-track-faces-priority-list
            (remq 'erc-notice-face erc-track-faces-priority-list))
  :custom (erc-track-priority-faces-only 'all))
(use-package erc-goodies
  :hook (erc-join . my-erc-enable-keep-place-indicator-on-join))
(use-package persistent-scratch
  :config (persistent-scratch-setup-default))
(defvar my-erc-read-indicator-channels '("#emacs")
  "Channels in which to show a `keep-place-indicator'.")
(defun my-erc-enable-keep-place-indicator-on-join ()
  "Enable read indicators for certain queries or channels."
  (when (member (erc-default-target) my-erc-read-indicator-channels)
    (erc-keep-place-indicator-mode +1)))
(defun erc-cmd-TRACK (&optional target)
  "Start tracking TARGET or that of current buffer."
  (setq erc-track-exclude
    (delete (or target (erc-default-target) (current-buffer))
      erc-track-exclude)))
(defun erc-cmd-UNTRACK (&optional target)
  "Stop tracking TARGET or that of current buffer."
  (setq erc-track-exclude
    (cl-pushnew (or target (erc-default-target) (current-buffer))
      erc-track-exclude
      :test #'equal)))

;; read epub files
(use-package nov)


(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))


(use-package minimal-theme)


(use-package remember
  :commands remember
  :bind ("C-c r" . remember)
  :config (setq remember-notes-initial-major-mode 'org-mode))

(use-package totp-auth)



(use-package tempel
  :bind
  ("s->" . tempel-end)
  ("s-}" . tempel-next)
  ("s-{" . tempel-previous)
  :hook (prog-mode . abbrev-mode)
  (prog-mode . tempel-abbrev-mode)
  (org-mode . abbrev-mode)
  (org-mode . tempel-abbrev-mode))

(use-package tempel-collection
  :after tempel)

(use-package olivetti)



(use-package elfeed
  :hook
  (elfeed-show-mode . visual-line-mode) ; make reading pretty
  (elfeed-show-mode . olivetti-mode   ) ; make reading pretty

  :config
  (progn
    (add-hook 'elfeed-show-mode-hook #'variable-pitch-mode)
    (add-hook 'elfeed-search-mode-hook #'variable-pitch-mode)
    (set-face-attribute 'variable-pitch nil
      :family "Noto Serif"
      :height 1.1)
    (set-fontset-font "fontset-default" 'devanagari
      "Noto Serif Devanagari")
    (setq elfeed-feeds
      '(
         "https://arthasarokar.com/feed"
         "https://bizpati.com/feed"
         "https://gorkhapatraonline.com/rss"
         "https://himalpress.com/feed/"
         "https://kendrabindu.com/feed"
         "https://khabarhub.com/feed/"
         "https://nagariknews.nagariknetwork.com/feed"
         "https://nepalnews.com/feed"
         "https://nepalsamaya.com/feed"
         "https://techmandu.com/feed/"
         "https://techsathi.com/feed"
         "https://thahakhabar.com/rss/"
         "https://ujyaaloonline.com/rss"
         "https://ukeraa.com/feed/"
         "https://www.ajakoartha.com/feed"
         "https://www.arthapath.com/feed/"
         "https://www.cinkhabar.com/feed"
         "https://www.eadarsha.com/feed"
         "https://www.himalkhabar.com/feed"
         "https://www.lokaantar.com/feed/"
         "https://www.onlinekhabar.com/feed"
         "https://www.ratopati.com/feed"
         "https://www.setopati.com/feed"
         "https://www.techpana.com/feed"
         ))))


(use-package spacious-padding
  :hook (fundamental-mode . spacious-padding-mode))


(use-package treemacs
  :bind ("s-z" . treemacs)
  :custom
  (treemacs-is-never-other-window t)
  :hook
  (treemacs-mode . treemacs-project-follow-mode))



(use-package gptel
  :ensure t
  :init
  (setq gptel-api-key 'gptel-api-key-from-auth-source)

  (setf (alist-get 'gemini gptel-backends)
        (gptel-make-gemini
         :model "gemini-2.5-flash" ; Use your preferred model
         :api-key gptel-api-key    ; This references the function set above
         :name "Gemini-Flash"))

  :config
  (setq gptel-default-model "gemini-2.5-flash"
        gptel-model "gemini-2.5-flash"))


(use-package pandoc)



(defun my-md-to-org-region (start end)
  "Convert region from markdown to org"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))
