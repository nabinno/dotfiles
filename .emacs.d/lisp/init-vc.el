;;; init-vc --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf diff-hl
  :ensure t
  :hook ((prog-mode-hook . turn-on-diff-hl-mode)
         (vc-dir-mode-hook . turn-on-diff-hl-mode))
  :config
  (global-diff-hl-mode +1)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  ;; For terminal use
  (unless (window-system)
    (diff-hl-margin-mode))
  ;; Force refresh
  (setq diff-hl-side 'left))


(provide 'init-vc)
;;; init-vc.el ends here
