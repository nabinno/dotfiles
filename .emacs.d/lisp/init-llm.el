;;; init-llm -- llm configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'leaf)
(leaf eat :ensure t)

(leaf claude-code
  :el-get stevemolitor/claude-code.el
  :config
  (claude-code-mode)
  (define-key global-map (kbd "C-c c") claude-code-command-map))

;; ;; Auto-revert configuration for claude-code
;; (setq auto-revert-interval 1)                ; Check every 1 second
;; (setq auto-revert-check-vc-info t)           ; Also check version control info
;; (setq global-auto-revert-non-file-buffers t) ; Revert non-file buffers too
;; (setq auto-revert-verbose nil)               ; Don't show messages when reverting

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


;; Copy context to Windows clipboard for WSL
(when (and (eq system-type 'gnu/linux)
           (getenv "WSL_DISTRO_NAME"))
  (defun claude-code-send-command-with-context-wsl ()
    "Send command to Claude Code and copy current context to Windows clipboard."
    (interactive)
    (let* ((current-line (line-number-at-pos))
           (buffer-name (buffer-name))
           (file-path (or (buffer-file-name) buffer-name))
           (context-info (format "%s:%d" file-path current-line))
           (region-text (if (use-region-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (thing-at-point 'line t)))
           (message-text (read-string "Message to Claude Code: "))
           (full-message (format "%s\n\nContext: %s\nCurrent line: %s"
                                 message-text context-info region-text)))
      ;; Copy to Windows clipboard using echo and PowerShell
      (let ((escaped-message (replace-regexp-in-string "'" "''" full-message)))
        (call-process "powershell.exe" nil nil nil
                      "-Command"
                      (format "[Console]::OutputEncoding = [System.Text.Encoding]::UTF8; echo '%s' | Set-Clipboard"
                              escaped-message)))
      (message "Context copied to Windows clipboard: %s" context-info)))

  ;; Bind key for WSL context copy
  (define-key claude-code-command-map (kbd "w") 'claude-code-send-command-with-context-wsl))


(provide 'init-llm)
;;; init-llm.el ends here
