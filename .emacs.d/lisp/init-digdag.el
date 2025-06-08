;;; init-digdag --- digdag configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf digdag :el-get syohex/emacs-digdag-mode)


(provide 'init-digdag)
;;; init-digdag.el ends here
