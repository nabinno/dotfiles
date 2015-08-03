;;; Neotree
(require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)

(add-hook 'neotree-mode-hook
          (lambda ()
           (define-key neotree-mode-map (kbd "i") 'neotree-enter)
           (define-key neotree-mode-map (kbd "j") 'neotree-next-line)
           (define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
           (define-key neotree-mode-map (kbd "K") 'neotree-create-node)
           (define-key neotree-mode-map (kbd "c") 'neotree-copy-node)
           (define-key neotree-mode-map (kbd "d") 'neotree-delete-node)
           (define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
           (define-key neotree-mode-map (kbd "f") 'isearch-forward)
           (define-key neotree-mode-map (kbd "p") 'beginning-of-buffer)
           (define-key neotree-mode-map (kbd "n") 'end-of-buffer))
          )


;; ;;; Projectile
;; (require-package 'project)
;; (projectile-global-mode)


(provide 'init-project)
