;;; init-golang -- basic golang configuration
;;; Commentary:
;;; Code:
(straight-use-package
  '(el-patch :type git :host github :repo "dominikh/go-mode.el"))
(use-package go-mode
  :mode "\\.go$"
  :straight t
  :init (if (executable-find "goimports")
            (setq gofmt-command "goimports")
          (message "Goimports not found; using default `gofmt-command'"))
  :config (add-hook 'before-save-hook 'gofmt-before-save))

(use-package eglot
  :straight t
  :config
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-,") 'pop-tag-mark)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-hook 'go-mode-hook 'eglot-ensure))

(use-package go-guru
  :after go
  :straight t
  :config (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package golint
  :after go
  :straight t)

(use-package go-eldoc
  :after go
  :straight t
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))



(provide 'init-golang)
;;; init-golang.el ends here
