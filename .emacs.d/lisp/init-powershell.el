;;; init-powershell --- PowerShell configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf powershell-mode :el-get jschaf/powershell.el)



(provide 'init-powershell)
;;; init-powershell.el ends here
