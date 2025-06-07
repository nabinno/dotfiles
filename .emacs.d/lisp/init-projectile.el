;;; init-projectile --- projectile configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (defun helm-projectile-ag ()
    "Connect to projectile."
    (interactive)
    (progn (helm-ag (projectile-project-root)) (delete-other-windows)))
  (define-key projectile-mode-map (kbd "C-c ; G") 'helm-projectile-ag)         ;; helm
  (define-key projectile-mode-map (kbd "C-c ; p") #'projectile-command-map)    ;; counsel
  (define-key projectile-mode-map (kbd "C-c ; p s r") 'counsel-projectile-rg)) ;; counsel


;; counsel-projectile
(leaf counsel-projectile
  :ensure t
  :after projectile ivy
  :config
  (defun counsel-rg-projectile (word)
    "Search WORD in projectile root directory."
    (interactive "srg: ")
    (progn (counsel-rg ivy-text default-directory))))

(with-eval-after-load "projectile"
  (when (require 'counsel-projectile nil t)
    (add-to-list 'counsel-projectile-switch-project-action
                 '("z" counsel-rg-projectile
                   "switch to rg") t)
    (add-to-list 'counsel-projectile-find-file-action
                 '("z" counsel-rg-projectile
                   "switch to rg") t)
    (setq projectile-completion-system 'ivy
          counsel-projectile-sort-files t
          counsel-projectile-sort-projects t)))

(provide 'init-projectile)
;;; init-projectile.el ends here
