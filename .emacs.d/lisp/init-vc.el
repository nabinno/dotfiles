;;; init-vc --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
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
  (setq diff-hl-side 'left)
  ;; Make diff-hl more visible like git-gutter
  (set-face-background 'diff-hl-change "purple")
  (set-face-foreground 'diff-hl-change "purple")
  (set-face-background 'diff-hl-insert "green")
  (set-face-foreground 'diff-hl-insert "green")
  (set-face-background 'diff-hl-delete "red")
  (set-face-foreground 'diff-hl-delete "red")
  ;; Use fringe for better visibility
  (diff-hl-margin-mode 1)
  ;; Key bindings similar to git-gutter
  (global-set-key (kbd "C-x n h") 'diff-hl-next-hunk)
  (global-set-key (kbd "C-x n p") 'diff-hl-previous-hunk)
  (global-set-key (kbd "C-x n v =") 'diff-hl-show-hunk)
  (global-set-key (kbd "C-x n r") 'diff-hl-revert-hunk))


(provide 'init-vc)
;;; init-vc.el ends here
