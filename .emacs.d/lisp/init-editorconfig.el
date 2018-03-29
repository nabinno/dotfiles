;;; init-editorconfig --- editorconfig configuration
;;; Commentary:
;;; Code:
(unless (require 'editorconfig nil 'noerror)
  (el-get-bundle editorconfig/editorconfig-emacs))
(editorconfig-mode 1)


(provide 'init-editorconfig)
;;; init-editorconfig.el ends here
