(use-package doom-themes :ensure t)
(use-package ef-themes :ensure t)

(use-package circadian
  :ensure t
  :after (doom-themes)
  :init
  (setq calendar-latitude 27.71)
  (setq calendar-longitude 85.32)
  (setq circadian-themes '((:sunrise . tango)
                           (:sunset . cyberpunk)))
  :config (circadian-setup))

(use-package nyx-theme :ensure )

(use-package hima-theme  :ensure t)

(use-package tao-theme
  :ensure t)

(use-package almost-mono-themes
  :ensure t)
