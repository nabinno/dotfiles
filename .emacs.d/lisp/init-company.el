;;; init-company -- company configuration
;;; Commentary:
;;; Code:
(require-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; (dolist (hook
;;          '(caml-mode-hook
;;            clojure-mode-hook
;;            crontab-mode-hook
;;            css-mode-hook
;;            emacs-lisp-mode-hook
;;            haskell-interactive-mode-hook
;;            haskell-mode-hook
;;            haskell-mode-hook
;;            inferior-haskell-mode-hook
;;            javascript-mode-hook
;;            js-mode-hook
;;            js2-mode-hook
;;            lisp-mode-hook
;;            nxml-mode-hook
;;            perl-mode-hook
;;            php-mode-hook
;;            python-mode-hook
;;            ruby-mode-hook
;;            scheme-mode-hook
;;            shell-mode-hook
;;            tcl-mode-hook
;;            yaml-mode
;;            ))
;;   (add-hook hook 'global-company-mode))



(provide 'init-company)
;;; init-company.el ends here
