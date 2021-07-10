;;; init-projectile --- projectile configuration
;;; Commentary:
;;; Code:
;;; Exlixr
(use-package projectile
  :straight (:host github :repo "bbatsov/projectile"))
;; (define-key projectile-mode-map projectile-keymap-prefix nil) ;;; TODO
(define-key projectile-mode-map (kbd "C-c ; p") #'projectile-command-map)
(projectile-global-mode)


(provide 'init-projectile)
;;; init-projectile.el ends here
