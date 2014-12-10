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
;; ;; yasnippet
;; (require-package 'yasnippet)
;; (yas-global-mode 1)
;; (yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rails/rails-snippets")
;; (yas-load-directory "~/.emacs.d/site-lisp/yasnippets-rspec/rspec-snippets")
;; (yas-reload-all)
;; (define-key yas-minor-mode-map (kbd "M-B") 'yas-insert-snippet)
;; (require-package 'dropdown-list)
;; (setq yas-prompt-functions '(yas-dropdown-prompt
;;                              yas-ido-prompt
;;                              yas-completing-prompt))

;; rspec-mode
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))
(require-package 'rspec-mode)

;; rinari-rgrep
(setq rinari-rgrep-file-endings "*.rb *.erb *.yml *.coffee *.js *.scss *.rake")
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories ".old")
     (add-to-list 'grep-find-ignored-directories ".sass-cache")
     (add-to-list 'grep-find-ignored-directories "bin")
     (add-to-list 'grep-find-ignored-directories "bower_components")
     (add-to-list 'grep-find-ignored-directories "commons")
     (add-to-list 'grep-find-ignored-directories "commons.min")
     (add-to-list 'grep-find-ignored-directories "db")
     (add-to-list 'grep-find-ignored-directories "fonts")
     (add-to-list 'grep-find-ignored-directories "log")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "vendor")))



(provide 'init-rails)
