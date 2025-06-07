;;; init-java.el --- basic java configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; IDE
;; malabar-mode
(leaf malabar-mode
  :el-get m0smith/malabar-mode
  :config
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
  (setq malabar-groovy-java-options '("-Duser.language=en")))

;; ;; jdee
;; (leaf jdee :ensure t)


;; Language Server Protocol
(leaf lsp-java :el-get emacs-lsp/lsp-java)


;;; Maven
(leaf mvn :ensure t)

;; javadoc
(leaf javadoc-lookup :ensure t)

;; Add and reorder Java import statements in Maven projects
(leaf javaimp :ensure t)

;; software testing
(leaf maven-test-mode :ensure t)


;;; Ant
(leaf ant :ensure t)

;; 
;; ;;; Groovy
;; (leaf groovy-mode :ensure t)
;; (add-auto-mode 'groovy-mode "\\.gradle\\'")


;;; Other
;; jtags
(leaf jtags :ensure t)

;; snipetts
(leaf java-snippets :ensure t)

;; thread dump
(leaf thread-dump :ensure t)



(provide 'init-java)
;;; init-java.el ends here
