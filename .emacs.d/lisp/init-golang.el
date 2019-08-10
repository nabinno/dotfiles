;;; init-golang -- basic golang configuration
;;; Commentary:
;;; Code:
(use-package go-mode
  :mode "\\.go$"
  :straight (el-patch :type git :host github :repo "dominikh/go-mode.el")
  :init (if (executable-find "goimports")
            (setq gofmt-command "goimports")
          (message "Goimports not found; using default `gofmt-command'"))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4))))

(use-package eglot
  :straight t
  :config
  (define-key eglot-mode-map (kbd "C-c ; g .") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c ; g ,") 'pop-tag-mark)
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

(use-package ginkgo-mode
  :after go
  :straight (el-patch :type git :host github :repo "garslo/ginkgo-mode"))




(provide 'init-golang)
;;; init-golang.el ends here
