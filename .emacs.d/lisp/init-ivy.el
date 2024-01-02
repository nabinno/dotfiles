;;; init-ivy --- ivy configuration
;;; Commentary:
;;; Code:
(leaf ivy :ensure t
  :config
  (setq ivy-initial-inputs-alist
        '((org-agenda-refile . "^")
          (org-capture-refile . "^")
          (Man-completion-table . "^")
          (woman . "^"))))


;; swiper
(leaf swiper
  :ensure t
  :config
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))


;; consel
(leaf counsel
  :ensure t
  :config
  (defun counsel-rg-dot-emacs (word)
    "Search WORD in .emacs.d directory."
    (interactive "srg: ")
    (progn (counsel-rg word (concat (getenv "DOTFILES_PATH") "/.emacs.d/"))))
  (defun counsel-rg-dot-zsh (word)
    "Search WORD in .zsh.d directory."
    (interactive "srg: ")
    (progn (counsel-rg word (concat (getenv "DOTFILES_PATH") "/.zsh.d/"))))
  (defun counsel-rg-dot-ghq (word)
    "Search WORD in .ghq.d directory."
    (interactive "srg: ")
    (progn (counsel-rg word "~/.ghq.d/")))
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x C-f") 'find-file)
  ;; (global-set-key (kbd "C-M-f") 'counsel-rg)
  ;; (global-set-key (kbd "C-c ; E") 'counsel-rg-dot-emacs)
  ;; (global-set-key (kbd "C-c ; H") 'counsel-rg-dot-ghq)
  ;; (global-set-key (kbd "C-c ; Z") 'counsel-rg-dot-zsh)
  )


;; ghq
(leaf ivy-ghq
  :el-get analyticd/ivy-ghq
  :if (executable-find "ghq")
  :commands (ivy-ghq-open)
  :custom
  (ivy-ghq-short-list nil))


;; magit
(with-eval-after-load "magit"
    (setq magit-completing-read-function 'ivy-completing-read))


;; ;; projectile
;; (unless (require 'counsel-projectile nil 'noerror)
;;   (el-get-bundle ericdanan/counsel-projectile))
;; (defun my-counsel-ag-in-default-dir (_arg)
;;   "Search the current directory with ag."
;;   (counsel-ag ivy-text default-directory))
;; (with-eval-after-load "projectile"
;;   (when (require 'counsel-projectile nil t)
;;     (add-to-list 'counsel-projectile-switch-project-action
;;                  '("z" my-counsel-ag-in-default-dir
;;                    "switch to ag") t)
;;     (add-to-list 'counsel-projectile-find-file-action
;;                  '("z" my-counsel-ag-in-default-dir
;;                    "switch to ag") t)
;;     (setq projectile-completion-system 'ivy)
;;     (setq counsel-projectile-sort-files t)
;;     (setq counsel-projectile-sort-projects t)
;;     (define-key projectile-mode-map (kbd "C-c ; p") 'projectile-command-map)
;;     (counsel-projectile-mode 1)))



(provide 'init-ivy)
;;; init-ivy.el ends here
