;;; init-java.el --- basic java configuration
;;; Commentary:
;;; Code:

;;; IDE
;; malabar-mode
(use-package malabar-mode
  :straight (:host github :repo "m0smith/malabar-mode"))
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
;; (use-package jdee :straight t)


;; Language Server Protocol
(straight-use-package
  '(el-patch :type git :host github :repo "emacs-lsp/lsp-java"))


;;; Maven
(use-package mvn :straight t)

;; javadoc
(use-package javadoc-lookup :straight t)

;; Add and reorder Java import statements in Maven projects
(use-package javaimp :straight t)

;; software testing
(use-package maven-test-mode :straight t)


;;; Ant
(use-package ant :straight t)

;; 
;; ;;; Groovy
;; (use-package groovy-mode :straight t)
;; (add-auto-mode 'groovy-mode "\\.gradle\\'")


;;; Other
;; jtags
(use-package jtags :straight t)

;; snipetts
(use-package java-snippets :straight t)

;; thread dump
(use-package thread-dump :straight t)



(provide 'init-java)
;;; init-java.el ends here
