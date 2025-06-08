;;; init-csharp -- basic csharp configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf csharp-mode
  :ensure t
  :config
  (add-hook 'csharp-mode-hook
            '(lambda()
               (setq c-basic-offset 4)
               (c-set-offset 'substatement-open 0)
               (c-set-offset 'case-label '+)
               (c-set-offset 'arglist-intro '+)
               (c-set-offset 'arglist-close 0)))
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'turn-on-eldoc-mode))


;;; OmniSharp
(leaf omnisharp
  :ensure t
  :after csharp-mode
  :config
  ;; (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (setq omnisharp-server-executable-path "~/.local/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
  (add-hook 'omnisharp-mode-hook
            (lambda ()
              (define-key omnisharp-mode-map "\C-c\C-s" 'omnisharp-start-omnisharp-server)
              (define-key omnisharp-mode-map "\M-/"     'omnisharp-auto-complete)
              (define-key omnisharp-mode-map "."        'omnisharp-add-dot-and-auto-complete)
              (define-key omnisharp-mode-map "\C-c\C-c" 'omnisharp-build-in-emacs)
              (define-key omnisharp-mode-map "\C-c\C-N" 'omnisharp-navigate-to-solution-member)
              (define-key omnisharp-mode-map "\C-c\C-n" 'omnisharp-navigate-to-current-file-member)
              (define-key omnisharp-mode-map "\C-c\C-f" 'omnisharp-navigate-to-solution-file)
              (define-key omnisharp-mode-map "\C-c\C-g" 'omnisharp-go-to-definition)
              (define-key omnisharp-mode-map "\C-c\C-r" 'omnisharp-rename)
              (define-key omnisharp-mode-map "\C-c\C-v" 'omnisharp-run-code-action-refactoring)
              (define-key omnisharp-mode-map "\C-c\C-o" 'omnisharp-auto-complete-overrides)
              (define-key omnisharp-mode-map "\C-c\C-u" 'omnisharp-helm-find-symbols)
              (define-key omnisharp-mode-map "\C-c\C-t\C-s" (lambda() (interactive) (omnisharp-unit-test "single")))
              (define-key omnisharp-mode-map "\C-c\C-t\C-r" (lambda() (interactive) (omnisharp-unit-test "fixture")))
              (define-key omnisharp-mode-map "\C-c\C-t\C-e" (lambda() (interactive) (omnisharp-unit-test "all")))))
  )


(provide 'init-csharp)
;;; init-csharp.el ends here
