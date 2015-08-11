;;; Basic java setup
You should have a section marked ";;; Commentary:"
The first line should be of the form: ";;; package --- Summary"
(setenv "JAVA_HOME" "/usr/lib/jvm/java-7-openjdk-amd64/")


;;; Malabar-mode
(require-package 'malabar-mode)
(add-auto-mode 'malabar-mode "\\.java\\'")
(when (require 'malabar-mode nil t)
  (setq malabar-groovy-lib-dir (expand-file-name "~/.emacs.d/malabar/lib"))
  (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
  ;; Exclude no-using packages from import lists
  (add-to-list 'malabar-import-excluded-classes-regexp-list "^java\\.awt\\..*$")
  (add-to-list 'malabar-import-excluded-classes-regexp-list "^com\\.sun\\..*$")
  (add-to-list 'malabar-import-excluded-classes-regexp-list "^org\\.omg\\..*$")
  (add-hook 'malabar-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'malabar-compile-file-silently
                        nil t))))
(after-load 'malabar-mode
  (add-hook 'malabar-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'malabar-mode-hook 'subword-mode))
(setq malabar-groovy-java-options '("-Duser.language=en"))


;;; Jtags
(require-package 'jtags)



(provide 'init-java)
