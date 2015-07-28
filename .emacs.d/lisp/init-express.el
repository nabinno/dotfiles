(unless (require 'jinari nil 'noerror)
  (el-get-bundle nabinno/jinari))

(after-load 'jinari
  (diminish 'jinari-minor-mode "Jin"))
(global-jinari-mode)

(defun update-express-ctags ()
  (interactive)
  (let ((default-directory (or (jinari-root) default-directory)))
    (shell-command
     (concat "ctags -a -e -f "
             jinari-tags-file-name
             " --langmap=JAVASCRIPT:.js --tag-relative -R node_modules src lib vendor test"))))


;;; Temporary Sets
;; GulpJs
(el-get-bundle stevenremot/emacs-gulpjs)

;; Jade-mode
(eval-after-load 'jade-mode
  (add-hook 'jade-mode-hook 'page-break-lines-mode))
(require-package 'jade-mode)

;; Jinari-rgrep
(setq jinari-rgrep-file-endings
      "*.rb *.erb *.yml *.yaml *.coffee *.js *.jade *.es6 *.json *.scss *.tag *.rake")
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


;;; Unit Testing
;; Karma
(require-package 'karma)

;; Jasmine-mode
(eval-after-load 'jasminejs-mode
  '(jasminejs-install-snippets))
(require-package 'jasminejs-mode)

;; Unit testing: Jst/Mocha
(require-package 'jst)
;; (add-hook 'js2-mode-hook 'jst-enable-appropriate-mode)
;; (add-hook 'coffee-mode-hook 'jst-enable-appropriate-mode)
;; (add-hook 'what-ever-js-mode-hook 'jst-enable-appropriate-mode)

;; ;; for node.js
;; (jst-declare-project :type "nodejs" :testing-framework "mocha"
;;                      :spec-dir nil :source-dir nil :command-ci nil
;;                      :command-browser nil :browser-url: nil
;;                      :target-to-spec (lambda () "testSuites.js")
;;                      :spec-to-target (lambda () "myLib.js"))

;; ;; If you created your own JS cluster language
;; (jst-remember-language :extension "qs" :name "MyQScript")
;; ;; If you name your spec dirs BlaBlaSuite
;; (jst-remember-spec-dir-pattern "\\(Suite\\)")
;; ;; If you are using darcs
;; (jst-remember-dominating-file ".darcs")


(provide 'init-express)
