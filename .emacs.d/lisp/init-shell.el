;;; init-shell -- shell configuration
;;; Commentary:
;;; Code:
(setq sh-basic-offset 2)
(setq sh-indentation 2)


;; Shellcheck
(add-hook 'sh-mode-hook 'flycheck-mode)


;; Shfmat
(defun shfmt-after-save ()
  "Run `shfmt -i 2 -ci -w' in Emacs."
  (if (derived-mode-p 'sh-mode)
      (shell-command (concat "shfmt -i 2 -ci -w " (buffer-file-name)))))
(add-hook 'after-save-hook 'shfmt-after-save)



(provide 'init-shell)
;;; init-shell.el ends here
