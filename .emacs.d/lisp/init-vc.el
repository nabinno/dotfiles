;;; init-vc --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'diff-hl)
(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)


(provide 'init-vc)
;;; init-vc.el ends here
