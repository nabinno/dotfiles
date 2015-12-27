(require-package 'php-mode)
(require-package 'smarty-mode)


;;; Inferior php
(require-package 'inf-php)
(require-package 'ac-inf-php)
(after-load 'auto-complete
  (add-to-list 'ac-modes 'inf-php-mode))
(add-hook 'inf-php-mode-hook 'ac-inf-php-enable)
(after-load 'inf-php
  (define-key inf-php-mode-map (kbd "TAB") 'auto-complete))



(provide 'init-php)
