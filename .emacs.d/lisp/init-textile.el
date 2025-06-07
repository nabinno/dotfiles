;;; init-teextile --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'textile-mode)

(autoload 'textile-mode "textile-mode" "Mode for editing Textile documents" t)
(setq auto-mode-alist
      (cons '("\\.textile\\'" . textile-mode) auto-mode-alist))


(provide 'init-textile)
;;; init-textile.el ends here
