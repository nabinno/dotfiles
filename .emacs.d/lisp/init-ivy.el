;;; init-ivy --- ivy configuration
;;; Commentary:
;;; Code:
(use-package ivy
  :straight t
  :config
  (setq ivy-initial-inputs-alist
        '((org-agenda-refile . "^")
          (org-capture-refile . "^")
          ;; (counsel-M-x . "^")
          (counsel-describe-function . "^")
          (counsel-describe-variable . "^")
          (Man-completion-table . "^")
          (woman . "^"))))


;; swiper
(use-package swiper
  :straight t
  :config
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))


;; consel
(use-package counsel
  :straight t
  :config
  (defun counsel-rg-dot-emacs (word)
    "Search .emacs.d directory."
    (interactive "srg: ")
    (progn (counsel-rg word (concat (getenv "DOTFILES_PATH") "/.emacs.d/"))))
  (defun counsel-rg-dot-zsh (word)
    "Search .zsh.d directory."
    (interactive "srg: ")
    (progn (counsel-rg word (concat (getenv "DOTFILES_PATH") "/.zsh.d/"))))
  (defun counsel-rg-dot-ghq (word)
    "Search .ghq.d directory."
    (interactive "srg: ")
    (progn (counsel-rg word "~/.ghq.d/")))
  (defun counsel-rg-dot-ghq-win32 (word)
    "Search .ghq.d directory on win32."
    (interactive "srg: ")
    (progn (counsel-rg word (concat (getenv "USER_PROFILE") "/.ghq.d/"))))
  (defun counsel-rg-projectile (word)
    "Connect to projectile."
    (interactive "srg: ")
    (counsel-rg word (projectile-project-root)))

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-M-f") 'counsel-rg)
  (global-set-key (kbd "C-c ; G") 'counsel-rg-projectile)
  (global-set-key (kbd "C-c ; E") 'counsel-rg-dot-emacs)
  (global-set-key (kbd "C-c ; H") 'counsel-rg-dot-ghq)
  (global-set-key (kbd "C-c ; W") 'counsel-rg-dot-ghq-win32)
  (global-set-key (kbd "C-c ; Z") 'counsel-rg-dot-zsh)

  ;; activate
  (counsel-mode 1))


;; ghq
(unless (require 'ivy-ghq nil 'noerror)
  (el-get-bundle analyticd/ivy-ghq))
(setq ivy-ghq-short-list t)


;; magit
(with-eval-after-load "magit"
    (setq magit-completing-read-function 'ivy-completing-read))


;; projectile
(unless (require 'counsel-projectile nil 'noerror)
  (el-get-bundle ericdanan/counsel-projectile))
(defun my-counsel-ag-in-default-dir (_arg)
  "Search the current directory with ag."
  (counsel-ag ivy-text default-directory))
(with-eval-after-load "projectile"
  (when (require 'counsel-projectile nil t)
    (add-to-list 'counsel-projectile-switch-project-action
                 '("z" my-counsel-ag-in-default-dir
                   "switch to ag") t)
    (add-to-list 'counsel-projectile-find-file-action
                 '("z" my-counsel-ag-in-default-dir
                   "switch to ag") t)
    (setq projectile-completion-system 'ivy)
    (setq counsel-projectile-sort-files t)
    (setq counsel-projectile-sort-projects t)
    (define-key projectile-mode-map (kbd "C-c ; p") 'projectile-command-map)
    (counsel-projectile-mode 1)))



(provide 'init-ivy)
;;; init-ivy.el ends here
