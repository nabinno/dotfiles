;;; init-haml --- haml configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf haml-mode :ensure t)

(after-load 'haml-mode
  (define-key haml-mode-map (kbd "C-o") 'open-line)
  (when (fboundp 'electric-indent-mode)
    (add-hook 'haml-mode-hook (lambda () (electric-indent-mode -1)))))

(provide 'init-haml)
;;; init-haml.el ends here
