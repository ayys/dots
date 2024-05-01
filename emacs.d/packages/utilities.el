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
  (("C-c m w" . mc/mark-all-symbols-like-this)
   ("C-c m e" . mc/edit-lines)
   ("C-c m r" . mc/mark-all-like-this)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package rg
  :ensure t
  :defer t
  :after (grep-mode)
  )

(use-package howm :ensure t
  :defer t
  :config
  (progn
    (setq howm-home-directory "~/docs/howm")
    (setq howm-directory "~/docs/howm/")
    (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
    (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
    (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
    (add-hook 'after-save-hook 'howm-mode-set-buffer-name)
    (define-key howm-menu-mode-map "\C-h" nil)
    (define-key riffle-summary-mode-map "\C-h" nil)
    (define-key howm-view-contents-mode-map "\C-h" nil)))

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
  :init
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
  :demand t                             ; Otherwise won't get loaded immediately
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
  (vertico-count 10)
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

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))



  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

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


(use-package totp :ensure t)


(use-package chatgpt-shell
  :defer t
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "openai-secret")))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package wrap-region
  :ensure t
  :defer t
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
