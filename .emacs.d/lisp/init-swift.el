;;; init-swift.el --- basic swift configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (require 'swift-mode nil 'noerror)
  (el-get-bundle swift-emacs/swift-mode))
(add-auto-mode 'swift-mode "\\.swift\\'")



(provide 'init-swift)
;;; init-swift.el ends here
