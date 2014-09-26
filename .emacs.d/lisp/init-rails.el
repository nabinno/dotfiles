(require-package 'rinari)
(after-load 'rinari
  (diminish 'rinari-minor-mode "Rin"))
(global-rinari-mode)

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))



; temporary sets
; ==============
;; yasnippet
(require-package 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rails/rails-snippets")
(yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rspec/rspec-snippets")
(yas-reload-all)
(define-key yas-minor-mode-map (kbd "M-B") 'yas-insert-snippet)
(require-package 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
;; rspec-mode
(require-package 'rspec-mode)
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))
(rspec-mode 1)



(provide 'init-rails)
