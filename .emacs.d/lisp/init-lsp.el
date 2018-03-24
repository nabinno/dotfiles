;;; init-lsp --- language server protocol configuration
;;; Commentary:
;;; Code:
(unless (require 'lsp-mode nil 'noerror)
  (el-get-bundle emacs-lsp/lsp-mode))


(provide 'init-lsp)
;;; init-lsp.el ends here
