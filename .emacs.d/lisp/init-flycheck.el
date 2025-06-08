;;; init-flycheck.el --- flycheck configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook '(lambda ()
                                   (when (and (buffer-file-name)
                                              (string= (file-name-base (buffer-file-name)) "magefile"))
                                     (flycheck-disable-checker 'go-errcheck)
                                     (flycheck-disable-checker 'go-build)
                                     )))

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; (leaf flycheck-tip :ensure t)


(provide 'init-flycheck)
;;; init-flycheck.el ends here
