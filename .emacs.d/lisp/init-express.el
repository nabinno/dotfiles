(unless (require 'jinari nil 'noerror)
  (el-get-bundle nabinno/jinari))

(after-load 'jinari
  (diminish 'jinari-minor-mode "Jin"))
(global-jinari-mode)

(defun update-express-ctags ()
  (interactive)
  (let ((default-directory (or (jinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " jinari-tags-file-name " --langmap=JAVASCRIPT:.js --tag-relative -R node_modules src lib vendor test"))))


; temporary sets
; ==============
;; jasmine-mode
(eval-after-load 'jasminejs-mode
  '(jasminejs-install-snippets))
(require-package 'jasminejs-mode)

;; gulpjs
(el-get-bundle stevenremot/emacs-gulpjs)

;; jade-mode
(require-package 'jade-mode)

;; jinari-rgrep
(setq jinari-rgrep-file-endings "*.rb *.erb *.yml *.yaml *.coffee *.js *.jade *.es6 *.json *.scss *.rake")
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
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "vendor")))



(provide 'init-express)
