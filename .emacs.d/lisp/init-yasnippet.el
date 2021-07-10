;;; init-yasnippet --- yasnippet configuration
;;; Commentary:
;;; COde:
(use-package yasnippet :straight t)

(yas-global-mode 1)
;; (yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rails/rails-snippets")
;; (yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rspec/rspec-snippets")
(yas-reload-all)
;; (define-key yas-minor-mode-map (kbd "M-B") 'yas-insert-snippet)

(unless (require 'dropdown-list nil 'noerror)
  (use-package dropdown-list :straight t))
;; (setq yas-prompt-functions '(yas-dropdown-prompt
;;                              yas-ido-prompt
;;                              yas-completing-prompt))

;; Keybind
(global-set-key (kbd "\C-c y") 'yas-insert-snippet)


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
