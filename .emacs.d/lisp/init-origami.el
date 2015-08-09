(require-package 'origami)
(add-hook 'after-init-hook 'origami-mode)
(add-hook 'prog-mode-hook (lambda () (interactive) (progn (origami-wrap-mode) (origami-cycle 1))))
;; (add-hook 'view-mode-hook 'view-mode-hook--origami)

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
