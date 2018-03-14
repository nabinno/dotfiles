;;; init-projectile --- projectile configuration
;;; Commentary:
;;; Code:
;;; Exlixr
(unless (require 'projectile nil 'noerror)
  (el-get-bundle bbatsov/projectile))
(setq projectile-keymap-prefix (kbd "C-c ; P"))
(projectile-global-mode)


(provide 'init-projectile)
;;; init-projectile.el ends here
