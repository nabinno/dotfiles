;;; init-llm -- llm configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf eat :ensure t)

(leaf claude-code
  :el-get stevemolitor/claude-code.el
  :config
  (claude-code-mode)
  (define-key global-map (kbd "C-c c") claude-code-command-map))

;; Auto-revert configuration for claude-code
(setq auto-revert-interval 1)                ; Check every 1 second
(setq auto-revert-check-vc-info t)           ; Also check version control info
(setq global-auto-revert-non-file-buffers t) ; Revert non-file buffers too
(setq auto-revert-verbose nil)               ; Don't show messages when reverting

;; (add-hook 'claude-code-start-hook 'auto-revert-mode)
;; (add-hook 'claude-code-mode-hook 'auto-revert-mode)
;; (global-auto-revert-mode 1)

;; Add specific revert behavior for claude-code buffers
(defun claude-code-force-revert ()
  "Force revert current buffer if it's a claude-code buffer."
  (interactive)
  (when (and (buffer-file-name)
             (or (derived-mode-p 'claude-code-mode)
                 (string-match "claude" (buffer-name))))
    (revert-buffer t t t)))

;; Bind key for manual refresh
(define-key claude-code-command-map (kbd "r") 'claude-code-force-revert)


(provide 'init-llm)
;;; init-llm.el ends here
