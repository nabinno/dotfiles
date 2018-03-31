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
(defun helm-projectile-ag ()
  "Connect to projectile."
  (interactive)
  (helm-ag (projectile-project-root)))
(global-set-key (kbd "C-c ; g") 'helm-projectile-ag)
(global-set-key (kbd "C-c ; E") 'helm-ag-dot-emacs)



(provide 'init-helm)
;;; init-helm.el ends here
