;;; init-origami -- origami configuration
;;; Commentary:
;;; Code:
(require-package 'origami)

(add-hook 'after-init-hook
          (lambda ()
            (progn
              (origami-mode)
              (dolist (hook
                       '(caml-mode-hook
                         clojure-mode-hook
                         crontab-mode-hook
                         css-mode-hook
                         emacs-lisp-mode-hook
                         haskell-interactive-mode-hook
                         haskell-mode-hook
                         haskell-mode-hook
                         inferior-haskell-mode-hook
                         javascript-mode-hook
                         js-mode-hook
                         js2-mode-hook
                         lisp-mode-hook
                         nxml-mode-hook
                         perl-mode-hook
                         php-mode-hook
                         python-mode-hook
                         ruby-mode-hook
                         scheme-mode-hook
                         shell-mode-hook
                         tcl-mode-hook
                         yaml-mode
                         ))
                (add-hook 'prog-mode-hook
                          (lambda ()
                            (progn (origami-wrap-mode)
                                   (origami-cycle 1)))))
              )))
;; (add-hook 'view-mode-hook 'view-mode-hook--origami)


;;; Functions
;; (makunbound 'origami-view-mode-map)
(define-minor-mode origami-wrap-mode
  "Setup origami folding for TAB"
  nil " 折"
  '(("\C-i" . origami-cycle))
  (or origami-mode (origami-mode 1)))
(defun origami-cycle (recursive)
  "Setup org-like origami function"
  (interactive "P")
  (call-interactively
   (if recursive 'origami-toggle-all-nodes 'origami-recursively-toggle-node)))
(defun origami-cycle-universally ()
  "Setup org-like origami function universally"
  (interactive)
  (progn
    (let ((recursive (universal-argument)))
      (origami-cycle recursive))))
;; (defun view-mode-hook--origami ()
;;   (when (memq major-mode (mapcar 'car origami-parser-alist))
;;     (origami-wrap-mode (if view-mode 1 -1))))

;; (defun my-amazing-parser (create)
;;   (lambda (content)
;;     (list (funcall create
;;                    beginning-of-the-fold-node-point-position ; inclusive
;;                    end-of-the-fold-node-point-position ; exclusive
;;                    offset  ; this allows you to show some of the start of the folded text
;;                    child-nodes))))


(provide 'init-origami)
;;; init-origami.el ends here
