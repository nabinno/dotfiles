(require-package 'paredit)
;; (autoload 'enable-paredit-mode "paredit")
(add-hook 'after-init-hook 'enable-paredit-mode)
(add-hook 'prog-mode-hook 'enable-paredit-mode)

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

(after-load 'paredit
  (diminish 'paredit-mode " Par")
  (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                         (kbd "C-M-<left>") (kbd "C-M-<right>")))
    (define-key paredit-mode-map binding nil))

  ;; Disable kill-sentence, which is easily confused with the kill-sexp
  ;; binding, but doesn't preserve sexp structure
  (define-key paredit-mode-map [remap kill-sentence] nil)
  (define-key paredit-mode-map [remap backward-kill-sentence] nil)

  ;; Allow my global binding of M-? to work when paredit is active
  (define-key paredit-mode-map (kbd "M-?") nil))


;; Compatibility with other modes
(suspend-mode-during-cua-rect-selection 'paredit-mode)

;; Use paredit in the minibuffer
;; TODO: break out into separate package
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))


;; Enable some handy paredit functions in all prog modes

(require-package 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)
(add-hook 'css-mode-hook 'paredit-everywhere-mode)
(after-load 'paredit-everywhere
    (define-key paredit-everywhere-mode-map [remap kill-sentence] 'paredit-kill))


;; Keybind
(add-hook 'paredit-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key paredit-mode-map
                        (read-kbd-macro key) func)))
                  '(("M-[ 1 ; 5 c" . paredit-backward-barf-sexp)
                    ("C-M-d" . paredit-forward-down)
                    ("M-[ 1 ; 5 d" . paredit-forward-slurp-sexp)
                    ("M-J" . paredit-join-sexps)
                    ("C-k" . paredit-kill)
                    ("M-\"" . paredit-meta-doublequote)
                    ("M-r" . paredit-raise-sexp)
                    ("M-s" . paredit-splice-sexp)
                    ("M-[ 1 ; 6 a" . paredit-splice-sexp-killing-backward)
                    ("M-S" . paredit-split-sexp)
                    ("M-" . paredit-wrap-curly)
                    ("M-(" . paredit-wrap-round)
                    ("M-[ [" . paredit-wrap-square)
                    ))))

;; Emacs Lisp
(fset 'paredit--next-block-elisp "\C-a\C-[\C-f\C-[\C-f\C-[\C-b")
(fset 'paredit--previous-block-elisp "\C-a\C-[\C-b")
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key paredit-mode-map
                        (read-kbd-macro key) func)))
                  '(("" . paredit--next-block-elisp)
                    ("" . paredit--previous-block-elisp)
                    ))))

;; JavaScript
(fset 'paredit--next-block-js "\C-a\C-[\C-f\C-[\C-f\C-[\C-f\C-[\C-f\C-[\C-b")
(fset 'paredit--previous-block-js "\C-a\C-[\C-b")
(add-hook 'js2-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key paredit-mode-map
                        (read-kbd-macro key) func)))
                  '(("" . paredit--next-block-js)
                    ("" . paredit--previous-block-js)
                    ))))



(provide 'init-paredit)
