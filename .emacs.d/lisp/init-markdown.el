(require-package 'markdown-mode)

(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "C-i") 'markdown-cycle)
                (hide-sublevels 2)))


(provide 'init-markdown)
