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
(after-load 'yasnippet
  (diminish 'yas-minor-mode "YAS"))
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/site-lisp/yasnippets-rails/rails-snippets/ruby-mode/"
;;        "~/.emacs.d/site-lisp/yasnippets-rails/rails-snippets/html-mode/"))
;; ido
(ido-mode t)
;; rspec-mode
(require-package 'rspec-mode)
(after-load 'rspec-mode
  (diminish 'rspec-mode "RSpec"))



(provide 'init-rails)
