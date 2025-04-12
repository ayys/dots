(setf (uiop/os:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1") 

(define-configuration (input-buffer)
  ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))


(define-configuration browser
  ((theme theme:+dark-theme+)))


(define-configuration web-buffer
  ((default-modes
    (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))


#+nyxt-2
(load-after-system :nx-search-engines (nyxt-init-file "search-engines.lisp"))
#+nyxt-3
(define-nyxt-user-system-and-load "nyxt-user/search-engines"
  :depends-on (:nx-search-engines) :components ("search-engines.lisp"))
