(require-package 'yasnippet)


(yas-global-mode 1)
;; (yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rails/rails-snippets")
;; (yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rspec/rspec-snippets")
(yas-reload-all)
;; (define-key yas-minor-mode-map (kbd "M-B") 'yas-insert-snippet)

(require-package 'dropdown-list)
;; (setq yas-prompt-functions '(yas-dropdown-prompt
;;                              yas-ido-prompt
;;                              yas-completing-prompt))



(provide 'init-yasnippet)
