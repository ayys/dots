(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(require 'package)
(package-initialize)
(require 'use-package)
(require 'linum)
(require 'bind-key)
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

(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

(global-unset-key (kbd "C-z"))

(setq byte-compile-warnings '(cl-functions))
(setq message-log-max t)
(savehist-mode 1)
(global-subword-mode)
(set-default 'cursor-type 'bar)

(let ((custom-file-path (expand-file-name "custom.el" user-emacs-directory)))
  (when (file-exists-p custom-file-path)
    (setq custom-file custom-file-path)))
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(add-hook 'prog-mode
          'electric-pair-mode)
(global-display-line-numbers-mode 1)
(display-time-mode -1)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super) ; make opt key do Super
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
(setq create-lockfiles nil)
(setq initial-scratch-message "")
;; (setq mouse-wheel-progressive-speed nil)

(setq initial-major-mode 'fundamental-mode)
;; (setq mouse-wheel-scroll-amount '(1))

(setq ispell-program-name (executable-find "hunspell")
      ispell-dictionary "en_US-large")
(setq-default line-spacing 2)
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))
(setq ispell-program-name "aspell")
(setq flyspell-default-dictionary "en_US")
(setq ispell-dictionary flyspell-default-dictionary)

(delete-selection-mode t)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(defun org-insert-backtick ()
  "Insert a backtick using `org-self-insert-command'."
  (interactive)
  (setq last-command-event ?`)
  (call-interactively #'org-self-insert-command))

(defun org-insert-tilde ()
  "Insert a tilde using `org-self-insert-command'."
  (interactive)
  (setq last-command-event ?~)
  (call-interactively #'org-self-insert-command))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(defun toggle-comment-on-line ()
  "COMMENT OR UNCOMMENT CURRENT LINE."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun copy-file-path-and-line-number-at-point ()
  "Copy the current buffer file name and line number at point to the clipboard."
  (interactive)
  (let ((file-path-and-line-number (format "%s::%d" (buffer-file-name) (line-number-at-pos))))
    (kill-new file-path-and-line-number)
    (message "Copied '%s'" file-path-and-line-number)))

(global-set-key (kbd "C-'") 'load-theme)
(global-set-key (kbd "C-c C-l") 'copy-file-path-and-line-number-at-point)
(global-set-key (kbd "C-:") 'goto-line)
;; (global-set-key (kbd "C-c C-m") 'projectile-compile-project)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-\"") 'disable-theme)
(global-set-key (kbd "C-c C-/") 'revert-buffer-no-confirm)
;; (global-set-key (kbd "C-c C-b")  'bookmark-set)
(global-set-key (kbd "C-c C-q")  'run-previous-comamnd-in-eshell)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "C-c C-s") 'rgrep)
(global-set-key (kbd "C-c I") 'irc)
;; (global-set-key (kbd "M-s") 'avy-goto-char-timer)
(global-set-key [C-mouse-wheel-down-event] 'text-scale-decrease)
(global-set-key [C-mouse-wheel-up-event]  'text-scale-increase)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.oरग\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("build.gradle" . groovy-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load custom-file)
(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/lisp")
(load "git")
(load "junk")
(load "markdown")
(load "programming")
(load "ayys-project")
(load "rust")
(load "terminal")
(load "themes")
(load "typo-theme")
(load "user-interface")
(load "utilities")


(add-to-list 'default-frame-alist '(font . "-*-Source Code Pro-regular-normal-normal-*-14-*-*-*-p-0-iso10646-1"))

;; (load-file "~/.emacs.d/navapali-maps.el")

(ido-mode 'buffers)  ;;  disable ido mode because it is annoying

(setq auth-sources '((:source "~/.authinfo.gpg")))
(setq epg-gpg-program "~/junk/gnupg/gnupg-2.4.0/bin/gpg")

(setq warning-minimum-level :emergency)

(setq visible-bell nil
      ring-bell-function 'ignore)


(setq display-time-day-and-date t)
(display-time)


;; map audio keys to manage text scale
(global-set-key (kbd "<XF86AudioLowerVolume>") 'text-scale-decrease)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'text-scale-increase)
(global-set-key (kbd "<XF86AudioMute>") 'text-scale-set)  ;; reset text scale to default




(global-set-key (kbd "M-[") 'beginning-of-buffer)
(global-set-key (kbd "M-]") 'end-of-buffer)

(setq x-select-enable-primary nil)
