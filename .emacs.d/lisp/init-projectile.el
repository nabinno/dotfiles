;;; init-projectile --- projectile configuration
;;; Commentary:
;;; Code:
(leaf projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c ; p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c ; G") 'projectile-grep))


;; counsel-projectile
(leaf counsel-projectile
  :ensure t
  :after projectile ivy
  :config (defun counsel-rg-projectile (word)
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
