;;; init-lsp --- language server protocol configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf lsp-mode
  :el-get "emacs-lsp/lsp-mode")


(provide 'init-lsp)
;;; init-lsp.el ends here
