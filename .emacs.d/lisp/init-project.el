;;; Sr Speedbar
(require-package 'sr-speedbar)
(global-set-key (kbd "M-2") 'sr-speedbar-toggle)

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-update-flag t
      sr-speedbar-width 20
      sr-speedbar-width-x 20
      sr-speedbar-auto-refresh t
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

;; keybind
(define-key speedbar-mode-map (kbd "p") 'speedbar-backward-list)
(define-key speedbar-mode-map (kbd "o") 'speedbar-contract-line-descendants)
(define-key speedbar-mode-map (kbd "+") 'speedbar-create-directory)
(define-key speedbar-mode-map (kbd "h") 'speedbar-edit-line)
(define-key speedbar-mode-map (kbd "m") '(progn (speedbar-next) (speedbar-edit-line t)))
(define-key speedbar-mode-map (kbd "i") 'speedbar-expand-line-descendants)
(define-key speedbar-mode-map (kbd "n") 'speedbar-forward-list)
(define-key speedbar-mode-map (kbd "c") 'speedbar-item-copy)
(define-key speedbar-mode-map (kbd "d") 'speedbar-item-delete)
(define-key speedbar-mode-map (kbd "?") 'speedbar-item-info)
(define-key speedbar-mode-map (kbd "r") 'speedbar-item-rename)
(define-key speedbar-mode-map (kbd "j") 'speedbar-next)
(define-key speedbar-mode-map (kbd "k") 'speedbar-prev)
(define-key speedbar-mode-map (kbd "g") 'speedbar-refresh)
(define-key speedbar-mode-map (kbd "J") 'speedbar-restricted-next)
(define-key speedbar-mode-map (kbd "K") 'speedbar-restricted-prev)
(define-key speedbar-mode-map (kbd "u") 'speedbar-scroll-down)
(define-key speedbar-mode-map (kbd "b") 'speedbar-scroll-up)
(define-key speedbar-mode-map (kbd " ") 'speedbar-toggle-line-expansion)
(define-key speedbar-mode-map (kbd "s") 'speedbar-toggle-sorting)
(define-key speedbar-mode-map (kbd "") 'speedbar-up-directory)
(define-key speedbar-mode-map (kbd "f") 'isearch-forward)
(define-key speedbar-mode-map (kbd "P") 'beginning-of-buffer)
(define-key speedbar-mode-map (kbd "N") 'end-of-buffer)
(define-key speedbar-mode-map (kbd "l") 'recenter)


;; ;;; Neotree
;; (require-package 'neotree)
;; (when (maybe-require-package 'neotree)
;;   (require 'neotree)
;;   (global-set-key (kbd "M-2") 'neotree-toggle))

;; (add-hook 'neotree-mode-hook
;;           (lambda ()
;;            (define-key neotree-mode-map (kbd "i") 'neotree-enter)
;;            (define-key neotree-mode-map (kbd "j") 'neotree-next-line)
;;            (define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
;;            (define-key neotree-mode-map (kbd "K") 'neotree-create-node)
;;            (define-key neotree-mode-map (kbd "c") 'neotree-copy-node)
;;            (define-key neotree-mode-map (kbd "d") 'neotree-delete-node)
;;            (define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
;;            (define-key neotree-mode-map (kbd "f") 'isearch-forward)
;;            (define-key neotree-mode-map (kbd "p") 'beginning-of-buffer)
;;            (define-key neotree-mode-map (kbd "n") 'end-of-buffer))
;;           )


;; ;;; Projectile
;; (require-package 'project)
;; (projectile-global-mode)


(provide 'init-project)
