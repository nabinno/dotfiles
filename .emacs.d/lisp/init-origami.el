;;; init-origami -- origami configuration
;;; Commentary:
;;; Code:
(leaf origami :ensure t)

(dolist (mode '(caml clojure crontab css emacs-lisp haskell-interactive haskell
                     haskell inferior-haskell lisp nxml perl
                     php python scheme tcl))
  (add-hook (intern (format "%s-mode-hook" mode))
            '(lambda ()
               (origami-mode)
               (origami-wrap-mode)
               (origami-cycle 1))))

;; (add-hook 'view-mode-hook 'view-mode-hook--origami)


;;; Functions
;; (makunbound 'origami-view-mode-map)
(define-minor-mode origami-wrap-mode
  "Setup origami folding for TAB"
  nil " 折"
  '(("\M-e" . origami-cycle))
  (or origami-mode (origami-mode 1)))
(defun origami-cycle (recursive)
  "Setup org-like origami function with RECURSIVE."
  (interactive "P")
  (call-interactively
   (if recursive 'origami-toggle-all-nodes 'origami-recursively-toggle-node)))
(defun origami-cycle-universally ()
  "Setup org-like origami function universally."
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
