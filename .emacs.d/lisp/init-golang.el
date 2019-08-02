;;; init-golang -- basic golang configuration
;;; Commentary:
;;; Code:
; @todo 2019-08-02
(use-package go-mode
  :mode "\\.go$"
  :straight t
  :init (if (executable-find "goimports")
            (setq gofmt-command "goimports")
          (message "Goimports not found; using default `gofmt-command'"))
  :config (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company-go
  :after go
  :disabled t)

(use-package go-guru
  :after go
  :straight t)



(provide 'init-golang)
;;; init-golang.el ends here
