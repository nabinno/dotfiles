;;; init-kotlin.el --- basic kotlin configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (require 'kotlin-mode nil 'noerror)
  (el-get-bundle Emacs-Kotlin-Mode-Maintainers/kotlin-mode))
(add-auto-mode 'kotlin-mode "\\.kt\\'")



(provide 'init-kotlin)
;;; init-kotlin.el ends here
