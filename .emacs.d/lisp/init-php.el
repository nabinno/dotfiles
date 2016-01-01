(require-package 'php-mode)
(require-package 'smarty-mode)


;;; Inferior php
(require-package 'inf-php)
(after-load 'inf-php
  (define-key inf-php-mode-map (kbd "TAB") 'auto-complete))



(provide 'init-php)
