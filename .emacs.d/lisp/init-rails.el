;;; init-rails --- rails configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf rinari
  :ensure t
  :config
  (after-load 'rinari
    (diminish 'rinari-minor-mode "Rin"))
  (global-rinari-mode))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))

;; rspec-mode
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))
(leaf rspec-mode :ensure t)

;; minitest-emacs
(eval-after-load 'minitest
  '(add-hook 'ruby-mode-hook 'minitest-mode))
(leaf minitest :ensure t)

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
;;; init-rails.el ends here
