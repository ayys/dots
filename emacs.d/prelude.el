;; Load custom file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

(setq use-package-always-ensure t)
(setq native-comp-deferred-compilation t)
(setq use-package-verbose t)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'text-scale-increase)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'text-scale-decrease)
(require 'linum)
;; Basic Settings
(setq org-confirm-babel-evaluate nil)

;; C-c <Right> <Left> to undo and redo window configuration
(tab-bar-history-mode)


(global-unset-key (kbd "C-z"))
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
(defun consult-vterm-buffer ()
  "Use consult-buffer to switch to a vterm buffer."
  (interactive)
  (consult-buffer '(consult--source-vterm)))
(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

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
(setq font-name "-APPL-Monaco-regular-normal-normal-*-*-*-*-*-*-0-iso10646-1")
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





(defun ayys/eat-terminal-split ()
  "Create a new Eat terminal in a split frame and switch to it.
If the current buffer is an Eat buffer, switch to the previous buffer instead."
  (interactive)
  (if (string-match-p "eat\\*$" (buffer-name))
      (switch-to-prev-buffer)
    (let ((split-window (split-window-below)))
      (select-window split-window)
      (eat-project))))


(defun ayys/eat-project (&optional arg)
  "
Fork of eat-project that optionally ignores the current project if
user is not in one.


Start Eat in the current project's root directory.

Start a new Eat session, or switch to an already active session.
Return the buffer selected (or created).

With a non-numeric prefix ARG, create a new session.

With a numeric prefix ARG (like
\\[universal-argument] 42 \\[eat-project]), switch to the session with
that number, or create it if it doesn't already exist."
  (interactive "P")
  (require 'project)
  (if-let* ((project-name (project-current nil)) (default-directory (project-root (project-current t))))
    (setq eat-buffer-name (project-prefixed-buffer-name "eat"))
    (setq eat-buffer-name "*eat*"))
  (eat nil arg))




(setq global-mark-ring-max 256)
(setq mark-ring-max 256)


(setq disabled-command-function nil) ;; enable all disabled commands


(global-hl-line-mode 0)
(set-mouse-color "purple")

(pixel-scroll-precision-mode 1) ;; Emacs 29+ only


(defun ayys/git-init-repo (dir)
  "Create a new Git repository in the specified DIR."
  (interactive "GDirectory to create new Git repo in: ")
  (unless (file-exists-p dir)
    (make-directory dir t))
  (let ((default-directory (file-name-as-directory dir)))
    (shell-command "git init")
    (message "Initialized empty Git repository in %s" dir)
    (dired dir)))


(defun background-brightness ()
  "Roughly determine brightness of current background using luminance formula."
  (let* ((bg (frame-parameter nil 'background-color))
         (rgb (color-values bg)) ;; returns (R G B) each in range 0-65535
         (r (/ (or (nth 0 rgb) 0) 65535.0))
         (g (/ (or (nth 1 rgb) 0) 65535.0))
         (b (/ (or (nth 2 rgb) 0) 65535.0)))
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun effective-theme-mode ()
  "Get effective background mode: 'light or 'dark."
  (or frame-background-mode
      (if (> (background-brightness) 0.5) 'light 'dark)))


(defun set-mouse-cursor-color-based-on-theme ()
  "Set mouse cursor color based on light/dark theme."
  (let ((mouse-color
          (pcase (effective-theme-mode)
            ('dark  "gold")      ;; Deep Sky Blue
            ('light "Royal Blue") ;; Crimson
            (_      "#FF00FF")))) ;; fallback: magenta
    (set-mouse-color mouse-color)))

(advice-add 'load-theme :after (lambda (&rest _)
                                 (set-mouse-cursor-color-based-on-theme)))
(advice-add 'disable-theme :after (lambda (&rest _)
                                 (set-mouse-cursor-color-based-on-theme)))
;; run set-mouse-cursor-color-based-on-theme on frame load
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (set-mouse-cursor-color-based-on-theme)))
;; Set initial mouse cursor color based on current theme
(set-mouse-cursor-color-based-on-theme)




(defun ayys/forge-insert-commit-titles-in-pr-buffer ()
  "Insert commit titles (no hashes) automatically in PR buffer."
  (when (string-match-p "new-pullreq" (buffer-name))
    (goto-char (point-max))
    (insert (shell-command-to-string "git log --pretty=format:'- %s' origin/main..HEAD"))))

(add-hook 'forge-create-pullreq-hook #'ayys/forge-insert-commit-titles-in-pr-buffer)
