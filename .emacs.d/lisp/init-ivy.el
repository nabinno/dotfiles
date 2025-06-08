;;; init-ivy --- ivy configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
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
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x C-f") 'find-file))


;; magit
(with-eval-after-load "magit"
    (setq magit-completing-read-function 'ivy-completing-read))



(provide 'init-ivy)
;;; init-ivy.el ends here
