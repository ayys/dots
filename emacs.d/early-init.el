(require 'package)

(setq package-archives nil) ;; nix handles this

(setq transient-mark-mode nil)

(defun spw/remap-mark-command (command &optional map)
  "Remap a mark-* command to temporarily activate Transient Mark mode."
  (let* ((cmd (symbol-name command))
         (fun (intern (concat "spw/" cmd)))
         (doc (concat "Call `"
                      cmd
                      "' and temporarily activate Transient Mark mode.")))
    (fset fun `(lambda ()
                 ,doc
                 (interactive)
                 (call-interactively #',command)
                 (activate-mark)))
    (if map
        (define-key map (vector 'remap command) fun)
      (global-set-key (vector 'remap command) fun))))

(dolist (command '(mark-word
                   mark-sexp
                   mark-paragraph
                   mark-defun
                   mark-page
                   mark-whole-buffer
                   rectangle-mark-mode))
  (spw/remap-mark-command command))

(with-eval-after-load 'org
  (spw/remap-mark-command 'org-mark-element org-mode-map)
  (spw/remap-mark-command 'org-mark-subtree org-mode-map))

;; sometimes a key to just activate the mark is wanted
(global-set-key "\M-i" (lambda () (interactive) (activate-mark)))
;; resettle the previous occupant
(global-set-key "\M-I" #'tab-to-tab-stop)

(setq mark-even-if-inactive t)
