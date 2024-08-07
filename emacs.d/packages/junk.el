;; (use-package svg-tag-mode
;;   :ensure t
;;   :hook ((prog-mode . svg-tag-mode) (org-mode . svg-tag-mode))
;;   :config
;;   (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
;;   (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
;;   (defconst day-re "[A-Za-z]\\{3\\}")
;;   (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))
;;   (setq svg-tag-tags
;;         `(
;;           (" :\\([a-zA-Z0-9\u0900-\u097F]+\\)" . ((lambda (tag) (svg-tag-make tag))))
;;           (" :\\([a-zA-Z0-9\u0900-\u097F]+[ \-]\\)" . ((lambda (tag) tag)))
;;           ("\\[#[a-zA-Z0-9\u0900-\u097F]+\\]" . ( (lambda (tag)
;;                                                     (svg-tag-make tag :face 'org-priority
;;                                                                   :beg 2 :end -1 :margin 0))))
;;           ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
;;           (,(format "\\(<%s>\\)" date-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :end -1 :margin 0))))
;;           (,(format "\\(<%s \\)%s>" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
;;           (,(format "<%s \\(%s>\\)" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))
;;           (,(format "\\(\\[%s\\]\\)" date-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
;;           (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
;;           (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
;;            ((lambda (tag)
;;               (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

(use-package centered-window  :ensure t )

;; (use-package company
;;   :ensure t
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-clang)
;;   (setq company-minimum-prefix-length 2)
;;   (setq company-idle-delay 0))
;; (use-package company-jedi
;;   :ensure t
;;   :after company
;;   :bind (("C-." . jedi:goto-definition)
;;          ("C-," . jedi:goto-definition-pop-marker))
;;   :config
;;   (defun my/python-mode-hook ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (setq jedi:setup-keys t)
;;   (setq jedi:complete-on-dot t)
;;   (add-hook 'python-mode-hook 'my/python-mode-hook))



;; (use-package rcirc
;;   :ensure t
;;   :config
;;   (setq rcirc-server-alist
;;         '(("irc.libera.chat"
;;            :user "ays"
;;            :port 6697 :encryption tls
;; 	   :channels ("#emacs"))
;;           ("localhost"
;;            :port 6668
;; 	   :channels ("#i2p"))))
;;   )


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


;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode)
;;   :ensure t)
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
  ;; :hook (dired-mode . dired-rainbow-mode)

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

;; (use-package emmet-mode

;;   :ensure t
;;   :hook web-mode)


(use-package yaml-pro
  :mode (
         ("\\.yml\\'" . yaml-pro-mode)
         ("\\.yaml\\'" . yaml-pro-mode))
  :ensure t)




;; (use-package paredit

;;   :ensure t
;;   :hook ( (scheme-mode  emacs-lisp-mode) . paredit-mode ))

(use-package git-gutter
  :ensure t
  :after magit
  :config
  (add-hook 'prog-mode-hook 'git-gutter-mode))



(use-package rainbow-delimiters

  :ensure t
  :hook ( prog-mode . rainbow-delimiters-mode ))
(use-package restclient
  :ensure t )

(use-package windmove
  :ensure t
  :bind (("C-c h" . windmove-left)
	 ("C-c l" . windmove-right)
	 ("C-c k" . windmove-up)
	 ("C-c j" . windmove-down)))


;; (use-package company-posframe :ensure t
;;   :config (company-posframe-mode t))

(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)


;; (use-package org-roam :ensure t
;;   :config (setq org-roam-directory "~/docs/roam")(org-roam-db-autosync-mode)
;;   :bind (("C-z C-i" . org-roam-node-insert)
;;          ("C-z C-f" . org-roam-node-find)
;;          ("C-z C-c" . org-roam-capture)
;;          ("C-z c" . org-roam-dailies-capture-today)))


(provide 'junk)
