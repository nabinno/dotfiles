;;; init-java.el --- basic java configuration
;;; Commentary:
;;; Code:

;;; IDE
;; malabar-mode
(unless (require 'malabar-mode nil 'noerror)
  (el-get-bundle m0smith/malabar-mode))
(add-auto-mode 'malabar-mode "\\.java\\'")
;; (when (require 'malabar-mode nil t)
;;   (setq malabar-groovy-lib-dir (expand-file-name "~/.emacs.d/malabar/lib"))
;;   (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;;   ;; Exclude no-using packages from import lists
;;   (add-to-list 'malabar-import-excluded-classes-regexp-list "^java\\.awt\\..*$")
;;   (add-to-list 'malabar-import-excluded-classes-regexp-list "^com\\.sun\\..*$")
;;   (add-to-list 'malabar-import-excluded-classes-regexp-list "^org\\.omg\\..*$")
;;   (add-hook 'malabar-mode-hook
;;             (lambda ()
;;               (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                         nil t))))
(after-load 'malabar-mode
  (add-hook 'malabar-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'malabar-mode-hook 'subword-mode))
(setq malabar-groovy-java-options '("-Duser.language=en"))

;; ;; jdee
;; (require-package 'jdee)


;; Language Server Protocol
(straight-use-package
  '(el-patch :type git :host github :repo "emacs-lsp/lsp-java"))


;;; Maven
(require-package 'mvn)

;; javadoc
(require-package 'javadoc-lookup)

;; Add and reorder Java import statements in Maven projects
(require-package 'javaimp)

;; software testing
(require-package 'maven-test-mode)


;;; Ant
(require-package 'ant)


;;; Kotlin
(require-package 'kotlin-mode)


;;; Groovy
(require-package 'groovy-mode)
(add-auto-mode 'groovy-mode "\\.gradle\\'")


;;; Other
;; jtags
(require-package 'jtags)

;; snipetts
(require-package 'java-snippets)

;; thread dump
(require-package 'thread-dump)



(provide 'init-java)
;;; init-java.el ends here
