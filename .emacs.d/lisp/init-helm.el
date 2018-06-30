;;; init-helm --- helm configuration
;;; Commentary:
;;; Code:
(require-package 'helm)


;; RipGrep
(require-package 'helm-ag)

(setq helm-ag-base-command "rg --vimgrep --no-heading")
(setq helm-ag-insert-at-point 'symbol) ; set up current symbol to default query
(defun helm-ag-dot-emacs ()
  "Search .emacs.d directory."
  (interactive)
  (helm-ag "~/.emacs.d/"))
(defun helm-ag-dot-zsh ()
  "Search .zsh.d directory."
  (interactive)
  (helm-ag "~/.zsh.d/"))
(defun helm-projectile-ag ()
  "Connect to projectile."
  (interactive)
  (helm-ag (projectile-project-root)))
(global-set-key (kbd "C-c ; G") 'helm-projectile-ag)
(global-set-key (kbd "C-c ; E") 'helm-ag-dot-emacs)
(global-set-key (kbd "C-c ; Z") 'helm-ag-dot-zsh)



(provide 'init-helm)
;;; init-helm.el ends here
