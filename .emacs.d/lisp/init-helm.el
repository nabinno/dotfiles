;;; init-helm --- helm configuration
;;; Commentary:
;;; Code:
(leaf helm :ensure t)


;; RipGrep
(leaf helm-ag
  :ensure t
  :config
  (setq helm-ag-base-command "rg --vimgrep --no-heading --hidden")
  (setq helm-ag-insert-at-point 'symbol) ; set up current symbol to default query
  (defun helm-ag-dot-emacs ()
    "Search .emacs.d directory."
    (interactive)
    (progn (helm-ag (concat (getenv "DOTFILES_PATH") "/.emacs.d/")) (delete-other-windows)))
  (defun helm-ag-dot-zsh ()
    "Search .zsh.d directory."
    (interactive)
    (progn (helm-ag (concat (getenv "DOTFILES_PATH") "/.zsh.d/")) (delete-other-windows)))
  (defun helm-ag-dot-ghq ()
    "Search .ghq.d directory."
    (interactive)
    (progn (helm-ag "~/.ghq.d/") (delete-other-windows)))
  (global-set-key (kbd "C-c ; E") 'helm-ag-dot-emacs)
  (global-set-key (kbd "C-c ; H") 'helm-ag-dot-ghq)
  (global-set-key (kbd "C-c ; Z") 'helm-ag-dot-zsh))



(provide 'init-helm)
;;; init-helm.el ends here
