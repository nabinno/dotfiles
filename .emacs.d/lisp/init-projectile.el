;;; init-projectile --- projectile configuration
;;; Commentary:
;;; Code:
;;; Exlixr
(unless (require 'projectile nil 'noerror)
  (el-get-bundle bbatsov/projectile)
  (require 'projectile))
(define-key projectile-mode-map projectile-keymap-prefix nil)
(define-key projectile-mode-map (kbd "C-c ; p") #'projectile-command-map)
(projectile-global-mode)


(provide 'init-projectile)
;;; init-projectile.el ends here
