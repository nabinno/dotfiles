;;; init-projectile --- projectile configuration
;;; Commentary:
;;; Code:
(leaf projectile
  :el-get bbatsov/projectile
  :config
  ;; (define-key projectile-mode-map projectile-keymap-prefix nil) ;;; TODO
  (define-key projectile-mode-map (kbd "C-c ; p") #'projectile-command-map)
  (projectile-global-mode))


(provide 'init-projectile)
;;; init-projectile.el ends here
