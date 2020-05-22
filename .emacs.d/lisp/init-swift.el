;;; init-swift.el --- basic swift configuration
;;; Commentary:
;;; Code:
(unless (require 'swift-mode nil 'noerror)
  (el-get-bundle swift-emacs/swift-mode))
(add-auto-mode 'swift-mode "\\.swift\\'")



(provide 'init-swift)
;;; init-swift.el ends here
