;;; init-proced -- proced configuration
;;; Commentary:
;;; Code:
(add-hook 'proced-mode-hook
          (lambda ()
            (progn
              (proced-toggle-auto-update 1)
              (proced-toggle-tree 1)
              (proced-sort-interactive 'args "")
              (define-key proced-mode-map (kbd "j") 'next-line)
              (define-key proced-mode-map (kbd "k") 'previous-line)
              (define-key proced-mode-map (kbd "p")
                (lambda ()
                  (interactive)
                  (progn (beginning-of-buffer) (next-line) (previous-line))))
              (define-key proced-mode-map (kbd "n")
                (lambda ()
                  (interactive)
                  (progn (end-of-buffer) (previous-line))))
              (define-key proced-mode-map (kbd "<SPC>") 'proced-mark)
              (define-key proced-mode-map (kbd "M-a") 'proced-unmark-all)
              (define-key proced-mode-map (kbd "d") 'proced-send-signal)
              (define-key proced-mode-map (kbd "D")
                (lambda ()
                  (interactive)
                  (progn (proced-send-signal "TERM")
                         (revert-buffer))))
              )))

;; keybinds
(global-set-key (kbd "") 'proced)



(provide 'init-proced)
;;; init-proced.el ends here
