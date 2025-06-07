;;; init-el-get --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Find and load the correct package.el

(require-package 'el-get)
(add-to-list 'load-path (expand-file-name "el-get" user-emacs-directory))


(provide 'init-el-get)
;;; init-el-get.el ends here
