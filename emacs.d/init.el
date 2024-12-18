;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(setq use-package-always-ensure t)
(setq native-comp-deferred-compilation t)

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


;; Basic Settings
(global-unset-key (kbd "C-z"))
(setq byte-compile-warnings '(cl-functions)
      message-log-max t
      create-lockfiles nil
      initial-scratch-message ""
      initial-major-mode 'fundamental-mode
      ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US-large"
      line-spacing 2
      auth-sources '((:source "~/.authinfo.gpg"))
      epg-gpg-program "~/junk/gnupg/gnupg-2.4.0/bin/gpg"
      warning-minimum-level :emergency
      visible-bell nil
      ring-bell-function 'ignore
      display-time-day-and-date t
      x-select-enable-primary nil)

;; UI and Behavior
(savehist-mode 1)
(global-subword-mode 1)
(global-display-line-numbers-mode 1)
(delete-selection-mode 1)
(set-default 'cursor-type 'bar)

;; Font stuff
(setq font-name "-*-Source Code Pro-regular-normal-normal-*-14-*-*-*-p-0-iso10646-1")
(setq-default frame-alist `((font . ,font-name)))
(add-to-list 'default-frame-alist `(font . ,font-name))

;; macOS Key Modifiers
(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      ns-function-modifier 'hyper)

;; Display Time
(display-time-mode 1)
(display-time)


;; disable menu-bar, scroll-bar and tool-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Ido mode disable
(ido-mode 'buffers)

(defun open-project-readme ()
  "Open the README.md file in the project root, ignoring case."
  (let ((root (projectile-project-root)))
    (when root
      (let ((readme (car (directory-files root t "\\`README\\.md\\'" t))))
        (when readme
          (find-file readme))))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

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
(defun my-fetch-all-forge-topics ()
  "Fetch all topics from the forge remote."
  (when (and (derived-mode-p 'forge-topic-mode)
             ;; Add any additional conditions to verify topic creation
             )
    (forge-pull)))

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

(require 'linum)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-clang)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0))

(use-package vterm-toggle
  :ensure t
  :config
  (progn
    (setq multi-vterm-dedicated-window-height-percent 30)
    (setq vterm-toggle-scope 'project))
  :bind (("C-c s v" . vterm-toggle))
  )

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

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

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
  :hook (prog-mode . yas-minor-mode)
  :ensure t)

(use-package yasnippet-snippets
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
;;; typo-theme.el --- Typographic (not color) Theme

;; Copyright (C) 2017 Bastian Bechtold

;; Author: Bastian Bechtold
;; URL: https://github.com/bastibe/.emacs.d/tree/master/lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A color theme without color. Like all text documents *except* source
;; code, this theme uses typography to distinguish between different
;; parts of text.
;;
;; Color-based highlighting is an anachronism borne from terminals'
;; inability to switch fonts. All we had was colors, so colors is what
;; we used. But in todays graphical world, this is no longer necessary,
;; and Emacs can use any font we like.
;;
;; I like PragmataPro. This theme is based on PragmataPro, and adds a
;; few other fonts for good measure. Strings are Iosevka Slab. Comments
;; are oblique Iosevka. Documentation is regular Iosevka. Headlines are
;; InputSerifCompressed. Ubuntu Mono works well, too.

;;; Credits:

;; Forked From:
;;
;; eink-emacs - Eink color theme for Emacs
;; Marian Schubert <marian.schubert@gmail.com>
;; http://github.com/maio/eink-emacs

;; Inspired by:
;;
;; https://bitbucket.org/kisom/eink.vim
;; https://github.com/dmand/eink.el
;; http://www.daveliepmann.stfi.re/tufte-css/?sf=wklwy

;;; Code:

(deftheme typo
  "Theme emulating reading on an E Ink device.")

(let* ((fg "#111111")
       (bg "#fffff8")
       (bg-light "#ddddd8")
       (fg-medium "#404040")
       (fg-light "#606060")
       (bg-lighter "#f4f4f0")
       (bg-white "#fcfcf8")
       (bg-highlight "#FFF1AA")
       (bg-highlight-2 "LightCyan")
       (bg-highlight-3 "LightGreen")
       (headline-1 `(:foreground ,fg :weight semi-bold :height 1.4 :family "InputSerifCompressed"))
       (headline-2 `(:foreground ,fg :weight semi-bold :height 1.4 :family "InputSerifCompressed"))
       (headline-3 `(:foreground ,fg :weight semi-bold :height 1.2 :family "Iosevka Slab"))
       (headline-4 `(:foreground ,fg :weight semi-bold :height 1.1)))


  (custom-theme-set-faces
   'typo

   ;; generic stuff
   `(default ((t (:background ,bg :foreground ,fg :family "PragmataPro"))))
   `(button ((t (:foreground ,fg :underline t))))
   `(cursor ((t (:background ,fg :foreground "white smoke"))))
   `(custom-variable-tag ((t (:foreground ,fg :weight bold))))
   `(default-italic ((t (:italic t))))
   `(font-lock-builtin-face ((t (:foreground ,fg-medium)))) ; nicht sichtbar
   `(font-lock-comment-delimiter-face ((t (:foreground ,fg :slant oblique :weight light :family "Iosevka"))))
   `(font-lock-comment-face ((t (:foreground ,fg :slant oblique :weight light :family "Iosevka"))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(font-lock-doc-face ((t (:foreground ,fg :weight light :family "Iosevka"))))
   `(font-lock-function-name-face ((t (:foreground ,fg :underline t))))
   `(font-lock-keyword-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,fg))))
   `(font-lock-reference-face ((t (:foreground ,fg))))
   `(font-lock-string-face ((t (:foreground ,fg-light :family "Iosevka Slab" :weight light)))) ; nicht sichtbar
   `(font-lock-type-face ((t (:foreground ,fg :underline t))))
   `(font-lock-variable-name-face ((t (:foreground ,fg-medium :underline nil)))) ; nicht sichtbar
   `(font-lock-warning-face ((t (:foreground ,fg :weight bold))))
   `(fringe ((t (:background ,bg :foreground ,fg))))
   `(gnus-header-content ((t (:foreground ,fg))))
   `(gnus-header-from ((t (:foreground ,fg))))
   `(gnus-header-name ((t (:foreground ,fg))))
   `(gnus-header-subject ((t (:foreground ,fg))))
   `(highlight ((t nil)))
   `(ido-first-match ((t (:foreground ,fg :weight bold))))
   `(ido-vertical-first-match ((t (:foreground ,fg :weight bold))))
   `(ido-only-match ((t (:foreground ,fg))))
   `(ido-subdir ((t (:foreground ,fg))))
   `(isearch ((t (:foreground ,fg :box (:line-width -1)))))
   `(isearch-lazy-highlight-face ((t (:foreground ,fg :box (:line-width -1)))))
   `(link ((t (:foreground ,fg))))
   `(minibuffer-prompt ((t (:foreground ,fg-medium :weight bold))))
   `(mode-line ((t (:background ,bg-light :foreground ,fg :height 1.0))))
   `(mode-line-buffer ((t (:foreground ,fg :weight bold))))
   `(mode-line-inactive ((t (:background ,bg-lighter :foreground ,fg-light :height 1.0))))
   `(mode-line-minor-mode ((t (:weight ultra-light))))
   `(modeline ((t (:background ,bg :foreground ,fg :height 1.0))))

   ;; latex
   `(font-latex-bold-face ((t (:foreground ,fg))))
   `(font-latex-italic-face ((t (:foreground ,fg :slant italic))))
   `(font-latex-match-reference-keywords ((t (:foreground ,fg))))
   `(font-latex-match-variable-keywords ((t (:foreground ,fg))))
   `(font-latex-string-face ((t (:foreground "#a9a9a9"))))
   `(font-latex-sectioning-5-face ((t (:foreground ,fg :weight bold))))
   `(font-latex-math-face ((t (:foreground ,fg))))
   `(font-latex-warning-face ((t (:foreground ,fg :weight bold))))
   `(font-latex-sedate-face ((t (:foreground ,fg :weight bold))))
   `(font-latex-sectioning-1-face ((t ,headline-1)))
   `(font-latex-sectioning-2-face ((t ,headline-2)))
   `(font-latex-sectioning-3-face ((t ,headline-3)))
   `(font-latex-sectioning-4-face ((t ,headline-4)))
   `(font-latex-sectioning-5-face ((t ,headline-4)))

   ;; org
   `(org-agenda-date ((t (:foreground ,fg :height 1.2))))
   `(org-agenda-date-today ((t (:foreground ,fg :weight bold :height 1.4))))
   `(org-agenda-date-weekend ((t (:foreground ,fg :weight normal))))
   `(org-agenda-structure ((t (:foreground ,fg :weight bold))))
   `(org-block ((t (:background ,bg-white :foreground ,fg))))
   `(org-block-background ((t (:background ,bg-white))))
   `(org-block-begin-line ((t (:foreground ,fg, :background ,bg-lighter :family "Iosevka Slab"))))
   `(org-block-end-line ((t (:foreground ,fg :background ,bg-lighter :family "Iosevka Slab"))))
   `(org-meta-line ((t (:foreground ,fg :background ,bg-lighter :family "Iosevka Slab"))))
   `(org-code ((t (:foreground ,fg-medium :background ,bg-white :family "Iosevka Slab"))))
   `(org-date ((t (:foreground ,fg) :underline)))
   `(org-hide ((t (:foreground ,bg))))
   `(org-document-title ((t ,headline-1)))
   `(org-document-info ((t (:foreground ,fg))))
   `(org-document-info-keyword ((t (:foreground ,fg-light :family "Iosevka Slab"))))
   `(org-level-1 ((t ,headline-2)))
   `(org-level-2 ((t ,headline-3)))
   `(org-level-3 ((t ,headline-4)))
   `(org-level-4 ((t ,headline-4)))
   `(org-level-5 ((t ,headline-4)))
   `(org-level-6 ((t ,headline-4)))
   `(org-link ((t (:foreground ,fg :underline t))))
   `(org-quote ((t (:foreground ,fg :slant italic :inherit org-block))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-sexp-date ((t (:foreground ,fg))))
   `(org-special-keyword ((t (:foreground ,fg))))
   `(org-todo ((t (:foreground ,fg :family "Iosevka Slab"))))
   `(org-done ((t (:foreground ,fg-light :family "Iosevka Slab"))))
   `(org-verse ((t (:inherit org-block :slant italic))))
   `(org-table ((t (:foreground ,fg))))

   `(region ((t (:background "#eeeee8" :foreground ,fg))))
   `(slime-repl-inputed-output-face ((t (:foreground ,fg))))
   `(whitespace-line ((t (:background ,bg-highlight-2))))
   `(whitespace-space ((t (:background ,bg :family "Iosevka"))))
   `(whitespace-newline ((t (:background ,bg :family "Iosevka"))))
   `(whitespace-empty ((t (:background ,bg :family "Iosevka"))))
   `(whitespace-trailing ((t (:background ,bg-highlight-2))))

   ;; magit
   `(magit-section-heading ((t (:weight bold :height 1.2))))
   `(magit-branch-local ((t (:weight bold))))
   `(magit-branch-remote ((t (:weight bold))))
   `(magit-branch-current ((t (:weight bold :box (:line-width -1)))))

   ;; markdown
   `(markdown-header-face-1 ((t ,headline-2)))
   `(markdown-header-face-2 ((t ,headline-3)))
   `(markdown-header-face-3 ((t ,headline-4)))
   `(markdown-header-face-4 ((t ,headline-4)))
   `(markdown-header-face-5 ((t ,headline-4)))
   `(markdown-header-face-6 ((t ,headline-4)))
   `(markdown-pre-face ((t (:foreground ,fg-medium :family "Iosevka Slab"))))
   `(markdown-inline-code-face ((t (:foreground ,fg-medium :family "Iosevka Slab"))))

   ;; compile
   `(compilation-error ((t (:inherit error))))

   ;; flycheck
   `(flycheck-error ((t (:inherit error))))
   `(flycheck-warning ((t (:inherit warning))))

   ;; dired
   `(dired-directory ((t (:weight bold))))
   `(dired-subtree-depth-1-face ((t (:background "grey90"))))

   ;; helm
   `(helm-source-header ((t (:foreground ,fg :background "grey90" :weight bold))))
   `(helm-header ((t (:foreground ,fg))))
   `(helm-selection-line ((t (:inherit region :weight bold))))
   `(helm-selection ((t (:background ,bg-highlight))))
   `(helm-ff-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,fg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,fg :slant italic))))
   `(helm-ff-executable ((t (:foreground ,fg))))

   ;; iedit
   `(iedit-occurrence ((t (:background ,bg-highlight-3 :foreground ,fg))))

   ;; company
   `(company-echo-common ((t (:foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,bg-highlight))))

   ;; parens - parenface
   '(parenface-paren-face ((t (:foreground "gray70"))))
   '(parenface-curly-face ((t (:foreground "gray70"))))
   '(parenface-bracket-face ((t (:foreground "gray70"))))

   ;; parens - paren-face
   '(parenthesis ((t (:foreground "gray70"))))

   ;; parens - other
   `(sp-show-pair-match-face ((t (:foreground "black" :weight bold :underline t))))
   `(sp-show-pair-mismatch-face ((t (:background "red" :foreground "black" :weight bold))))
   `(show-paren-match ((t (:foreground "black" :weight bold :underline t))))
   `(show-paren-mismatch ((t (:background "red" :foreground "black" :weight bold))))

   ;; js2
   `(js2-function-param ((t (:foreground ,fg))))
   `(js2-external-variable ((t (:foreground ,fg))))

   ;; perl
   `(cperl-hash-face ((t (:foreground ,fg))))
   `(cperl-array-face ((t (:foreground ,fg))))
   `(cperl-nonoverridable-face ((t (:foreground ,fg))))

   ;; rpm-spec-mode
   `(rpm-spec-tag-face ((t (:inherit default))))
   `(rpm-spec-package-face ((t (:inherit default))))
   `(rpm-spec-macro-face ((t (:inherit default))))
   `(rpm-spec-doc-face ((t (:inherit default))))
   `(rpm-spec-var-face ((t (:inherit default))))
   `(rpm-spec-ghost-face ((t (:inherit default))))
   `(rpm-spec-section-face ((t (:inherit default :weight bold))))

   ;; misc
   `(idle-highlight ((t (:background ,bg-highlight))))
   `(yas-field-highlight-face ((t (:background "#eeeee8" :foreground ,fg))))
   `(eshell-prompt ((t (:foreground ,fg :weight bold))))
   `(cider-result-overlay-face ((t (:weight bold))))))

;;;###autoload
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

(defun vterm-all-names ()
  (let ((buffer-names (mapcar #'buffer-name (buffer-list))))
    (seq-filter (lambda (name) (or (s-contains-p "vterm" name) (s-contains-p "eshell" name))) buffer-names)))

(defvar consult--source-vterm
  `(:name     "Vterm"
    :narrow   ?m
    :category vterm
    :face     consult-bookmark
    :state    ,#'consult--buffer-state
    :items    ,#'vterm-all-names)
  "Bookmark candidate source for `consult-buffer'.")

(defun consult-vterm-buffer ()
  "Use consult-buffer to switch to a vterm buffer."
  (interactive)
  (consult-buffer '(consult--source-vterm)))

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

(use-package devil :ensure t
  :init (global-devil-mode))

(use-package separedit :ensure t
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

(setq use-package-verbose t)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :bind (("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion))
  :hook (prog-mode . copilot-mode))


(use-package string-inflection
  :ensure t
  :bind ("C-c C-u" . string-inflection-all-cycle)  
  )


