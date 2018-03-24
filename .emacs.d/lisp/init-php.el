;;; init-php --- php configuration
;;; Commentary:
;;; Code:
(require-package 'php-mode)
(require-package 'smarty-mode)


;; Language Server Protocol
(unless (require 'lsp-php nil 'noerror)
  (el-get-bundle emacs-lsp/lsp-php))



(provide 'init-php)
;;; init-php.el ends here
