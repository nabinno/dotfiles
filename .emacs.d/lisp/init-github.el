;;; init-github --- github configuration
;;; Commentary:
;;; Code:
(require 'init-git)

(leaf yagist :ensure t)
(leaf github-browse-file :ensure t)
(leaf bug-reference-github
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode))

(leaf ido-completing-read+ :ensure t)
;; (leaf github-clone :ensure t)
(leaf magit-gh-pulls
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))


;; ;;; magithub
;; (leaf magithub
;;   :ensure t
;;   :config
;;   (defun require-magithub ()
;;     "Requre magithub."
;;     (require 'magithub))
;;   (add-hook 'magit-mode-hook 'require-magithub))


;;; git-link
(leaf git-link :ensure t)



(provide 'init-github)
;;; init-github.el ends here
