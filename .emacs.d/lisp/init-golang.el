;;; init-golang -- basic golang configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf go-mode
  :mode "\\.go$"
  :el-get dominikh/go-mode.el
  :commands go-mode
  :defer-config
  (if (executable-find "goimports")
      (setq gofmt-command "goimports")
    (message "Goimports not found; using default `gofmt-command'"))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4))))

(leaf company-go
  :ensure t
  :config
  (define-key go-mode-map (kbd "M-N") 'company-go))

(leaf eglot
  :ensure t
  :config
  (define-key eglot-mode-map (kbd "C-c ; g .") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "C-c ; g ,") 'pop-tag-mark)
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-hook 'go-mode-hook 'eglot-ensure))

(leaf go-guru
  :after go
  :ensure t
  :config (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(leaf golint
  :after go
  :ensure t)

(leaf go-eldoc
  :after go
  :ensure t
  :config (add-hook 'go-mode-hook 'go-eldoc-setup))

(leaf ginkgo-mode
  :el-get garslo/ginkgo-mode)



(provide 'init-golang)
;;; init-golang.el ends here
