;; Load custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))


(require 'use-package)
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



(add-to-list 'load-path "~/.emacs.d/packages")
(load "typo-theme")
(load "themes")
(load "git")
(load "junk")
(load "markdown")
(load "programming")
(load "ayys-project")
(load "terminal")
(load "user-interface")
(load "utilities")
