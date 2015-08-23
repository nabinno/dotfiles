;;; init-proced -- proced configuration
;;; Commentary:
;;; Code:
(defun proced-settings ()
  "Setup proced configuration."
  (interactive)
  (progn
    (proced-toggle-auto-update)
    (proced-toggle-tree)
    (proced-sort-interactive "args")
    (define-key proced-mode-map (kbd "j") 'next-line)
    (define-key proced-mode-map (kbd "k") 'previous-line)
    (define-key proced-mode-map (kbd " ") 'proced-mark)
    (define-key proced-mode-map (kbd "M-a") 'proced-unmark-all)
    (define-key proced-mode-map (kbd "d") 'proced-send-signal)
    (define-key proced-mode-map (kbd "D") (lambda () (proced-send-signal "TERM")))
    ))
(add-hook 'proced-mode-hook 'proced-settings)

;; keybinds
(global-set-key (kbd "") 'proced)



(provide 'init-proced)
;;; init-proced.el ends here
