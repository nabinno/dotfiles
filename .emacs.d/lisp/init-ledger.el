;;; init-ledger --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(when (> emacs-major-version 23)
  (require-package 'flycheck-ledger))
(after-load 'flycheck
  (require 'flycheck-ledger))
(after-load 'ledger-mode
  (define-key ledger-mode-map (kbd "RET") 'newline)
  (define-key ledger-mode-map (kbd "C-o") 'open-line))

(setq ledger-highlight-xact-under-point nil
      ledger-use-iso-dates nil)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "LEDGER_FILE"))

(add-hook 'ledger-mode-hook 'goto-address-prog-mode)

(provide 'init-ledger)
;;; init-ledger.el ends here
