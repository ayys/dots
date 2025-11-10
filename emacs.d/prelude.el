;; Load custom file  -*- lexical-binding: t; -*-

(require 's)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :no-error-if-file-is-missing)

(setq use-package-always-ensure t)
(setq native-comp-deferred-compilation t)
(setq use-package-verbose t)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'text-scale-increase)
(global-set-key (kbd "<XF86AudioLowerVolume>") 'text-scale-decrease)

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
  epg-gpg-program (executable-find "gpg")
  warning-minimum-level :emergency
  visible-bell nil
  ring-bell-function 'ignore
  display-time-day-and-date t
  x-select-enable-primary nil)
;; UI and Behavior
(savehist-mode 1)
(global-subword-mode 1)
(global-display-line-numbers-mode 1)
(setopt display-line-numbers-width 3)           ; Set a minimum width
(add-hook 'text-mode-hook 'visual-line-mode)


(delete-selection-mode 1)
(set-default 'cursor-type 'bar)
;; Font stuff
(setq font-name "-SAJA-Cascadia Mono-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1")
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

  (let ((init-dir default-directory))  ;; init-dir is where eat should launch
    (if-let* ((project-name (project-current nil))
               (project-root (project-root project-name)))
      (setq init-dir project-root
        eat-buffer-name (project-prefixed-buffer-name "eat"))
      (setq eat-buffer-name "*eat*"))
    (let ((default-directory init-dir))
      (eat nil arg))))


(defun ayys/consult-project-eat-buffers (&optional force-consult)
  "List and switch to open project 'eat' buffers.
If no 'eat' buffers exist, create one. If buffers exist, allows
the user to optionally create a new one as well."
  (interactive "P")
  (require 'project)
  (require 'consult)

  (let* ((project-name (project-current nil))
          (project-buffers (and project-name (project-buffers project-name)))
          (eat-buffers (cl-remove-if-not
                         (lambda (buf)
                           (with-current-buffer buf
                             (and (eq major-mode 'eat-mode)
                               (member buf project-buffers))))
                         (buffer-list)))
          (existing-source `( :name "Eat buffers in current project"
                              :category buffer
                              :action (lambda (buffer) (switch-to-buffer buffer))
                              :items ,(mapcar 'buffer-name eat-buffers)))
          (create-source `(:name "Create new shell"
                            :items ("(Create new eat buffer for project)")
                            :hidden: nil
                            :action (lambda (_) (ayys/eat-project t))))
          (exit-eat-source `(:name "Exit out of eat"
                              :items ("(Exit eat)")
                              :hidden: nil
                              :action (lambda (_) (switch-to-prev-buffer))))
          (sources (append (if (eq major-mode 'eat-mode) (list exit-eat-source) nil) (list existing-source create-source))))
    (cond
      ;; case -1: force-consult is true
      (force-consult (consult-buffer sources))
      ;; case 0: currently in eat buffer
      ((eq major-mode 'eat-mode) (switch-to-prev-buffer))
      ;; case 1: currently not in project
      ((null project-name) (ayys/eat-project))
      ;; case 2: in project, but no eat buffers exist yet
      ((null eat-buffers) (ayys/eat-project))
      ;; case 3: there is one eat buffer
      ((eq (length eat-buffers) 1) (switch-to-buffer (car eat-buffers)))
      ;; case 4: multiple buffers exist. list existing and offer a create option too
      (t (consult-buffer sources)))))

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

;; world-clock config
(setq zoneinfo-style-world-list
  '(("America/Los_Angeles" "SanFran")
     ("Asia/Calcutta" "India")
     ("Asia/Kathmandu" "Kathmandu")))

(setq world-clock-time-format "%T\t%Z\t%d %b\t%A")
(global-set-key (kbd "C-x <prior>") 'world-clock)



;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)


;; Fix archaic defaults
(setopt sentence-end-double-space nil)


;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))


(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
          (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
          (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion


(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell





;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))



(use-package whitespace
  :demand t ; Ensures the package is loaded immediately if you want the keybinding to work right away
  :config
  (keymap-global-set "C-c w" 'whitespace-mode))

;; Keybindings for built-in or already-loaded functions can also be set directly:
(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
                                        ;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(blink-cursor-mode t)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-dont-pause t)
(setq jit-lock-defer-time 0.05)
;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well
(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent
(setopt show-trailing-whitespace t)        ; By default, underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)
(setq redisplay-skip-fontification-on-input t)

(use-package emacs
  :bind (("C-'" . load-theme)
          ("C-\"" . disable-theme)
          ("M-j" . duplicate-dwim)
          ("C-c C-/" . revert-buffer-no-confirm)
          ("C-:" . goto-line)))





(progn ;; a neat and tidy modeline (uses minions)
  (use-package minions) ;; to hide minor modes; they are useless 99.9999999% of the time

  ;; credit: https://gist.github.com/mmarshall540/e420f6848e39e45c6981e0f0418f5ea2
  (setopt mode-line-format
    '("%e"
       mode-line-front-space
       ;; removed mode-line-mule-info
       mode-line-client
       mode-line-modified
       mode-line-remote
       mode-line-window-dedicated
       ;; removed `display' property from the above constructs
       "\t"                             ; added
       mode-line-frame-identification
       mode-line-buffer-identification
       "   "
       mode-line-position
       mode-line-format-right-align
       (project-mode-line project-mode-line-format)
       (vc-mode vc-mode)
       "  "
       minions-mode-line-modes
       mode-line-misc-info
       "  "                        
       mode-line-end-spaces))
  (setopt mode-line-modified
    '((:eval (if buffer-read-only "R" ""))
       (:propertize
         (:eval (if (buffer-modified-p) "×" "")) face warning)))
  (setopt mode-line-modes (remove "(" (remove ")" mode-line-modes)))
  (setopt mode-line-position-column-line-format '("%l:%c"))
  (setopt mode-line-position-line-format '("L%l"))
  (setopt mode-line-remote
    '(:eval (if (file-remote-p default-directory) "☎" "")))
  (setopt mode-line-right-align-edge 'window)
  (setq display-time-format "%0H:%0M:%0S")
  (setq display-time-interval 1)
  (display-time-mode 1)
  (setq display-time-default-load-average nil)
  )
