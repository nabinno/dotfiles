;;; init-github --- github configuration
;;; Commentary:
;;; Code:
(require 'init-git)

(use-package yagist :straight t)
(use-package github-browse-file :straight t)
(use-package bug-reference-github
  :straight t
  :config
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(use-package ido-completing-read+ :straight t)
;; (use-package github-clone :straight t)
(use-package magit-gh-pulls
  :straight t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))


;;; magithub
;; (use-package magithub :straight t)
;; (defun require-magithub ()
;;   "Requre magithub."
;;   (require 'magithub))
;; (add-hook 'magit-mode-hook 'require-magithub)


;;; git-link
(straight-use-package 'git-link)



(provide 'init-github)
;;; init-github.el ends here
