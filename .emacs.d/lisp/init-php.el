;;; init-php --- php configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf php-mode :ensure t)
(leaf smarty-mode :ensure t)


;; ;; Language Server Protocol
;; (leaf lsp-php
;;   :el-get emacs-lsp/lsp-php)



(provide 'init-php)
;;; init-php.el ends here
