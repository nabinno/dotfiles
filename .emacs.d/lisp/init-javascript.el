;;; init-javascript -- javascript configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf json-mode :ensure t)
(leaf js2-mode :ensure t :emacs>= 24)
(leaf ac-js2 :ensure t :emacs>= 24)
(leaf coffee-mode :ensure t :emacs>= 24)
(leaf js-comint :ensure t)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(rjsx-mode js-mode js2-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(rjsx-mode js-mode js2-mode))
(defvar preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist
      (append '(("\\.js\\(\\.erb\\)?\\'" . rjsx-mode))
              auto-mode-alist))
;; (setq auto-mode-alist (cons `("\\.js\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
;;                             (loop for entry in auto-mode-alist
;;                                   unless (eq preferred-javascript-mode (cdr entry))
;;                                   collect entry)))
(setq auto-mode-alist (cons `("\\.es6\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;;; Prettier
(leaf prettier-js :el-get prettier/prettier-emacs)
(leaf add-node-modules-path :el-get codesuki/add-node-modules-path)
(eval-after-load 'typescript-mode
  '(progn
     (add-hook 'typescript-mode-hook #'add-node-modules-path)
     (add-hook 'typescript-mode-hook #'prettier-js-mode)))
(eval-after-load 'js-mode
  '(progn
     (add-hook 'js-mode-hook #'add-node-modules-path)
     (add-hook 'js-mode-hook #'prettier-js-mode)))
(eval-after-load 'js2-mode
  '(progn
     (add-hook 'js2-mode-hook #'add-node-modules-path)
     (add-hook 'js2-mode-hook #'prettier-js-mode)))
(eval-after-load 'rjsx-mode
  '(progn
     (add-hook 'rjsx-mode-hook #'add-node-modules-path)
     (add-hook 'rjsx-mode-hook #'prettier-js-mode)))


;;; Js-mode
(after-load 'js-mode
  (add-hook 'js-mode-hook '(lambda () (setq mode-name "JS"))))

(setq-default js-indent-level preferred-javascript-indent-level)

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


;;; Js2-mode
(after-load 'js2-mode
  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2"))))

(setq-default
 js2-basic-offset preferred-javascript-indent-level
 js2-bounce-indent-p nil)

(after-load 'js2-mode (js2-imenu-extras-setup))


;; ;; Language Server Protocol
;; (leaf lsp-javascript
;;   : "emacs-lsp/lsp-javascript"))


;; ;;; Repl: Babel, Node.js
;; (leaf babel-repl :ensure t)
;; (leaf nodejs-repl :ensure t)


;;; Company-tern
(leaf company-tern
  :el-get kevinushey/company-tern
  :config
  (setq company-tern-property-marker "")

  (defun company-tern-depth (candidate)
    "Return depth attribute for CANDIDATE, 'nil' entries are treated as 0."
    (let ((depth (get-text-property 0 'depth candidate)))
      (if (eq depth nil) 0 depth)))

  (dolist (hook '(js-mode-hook js2-mode))
    (add-hook hook '(lambda ()
                      (tern-mode)
                      (add-to-list 'company-backends '(company-tern :with company-dabbrev-code)))))
  (eval-after-load 'tern
    '(progn
       (define-key tern-mode-keymap (kbd "M-,") 'mc/mark-previous-like-this)
       (define-key tern-mode-keymap (kbd "M-.") 'mc/mark-next-like-this))))


;; Javascript nests {} and () a lot, so I find this helpful
(leaf rainbow-delimiters
  :ensure t
  :config
  (dolist (hook '(js2-mode-hook js-mode-hook json-mode-hook))
    (add-hook hook 'rainbow-delimiters-mode)))


;;; CoffeeScript
(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))


;;; TypeScript
(leaf typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2))

;; Tide (npm i -g typescript)
(leaf tide
  :ensure t
  :config
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  (add-hook 'typescript-mode-hook #'tide-setup)
  (with-eval-after-load 'tide
    (define-key tide-mode-map (kbd "M-o M-,") 'tide-jump-to-definition)
    (define-key tide-mode-map (kbd "M-o M-.") 'tide-jump-back)
    (define-key tide-mode-map (kbd "C-c C-d") 'tide-restart-server)
    (define-key tide-mode-map (kbd "M-,")   'mc/mark-previous-like-this)
    (define-key tide-mode-map (kbd "M-.")   'mc/mark-next-like-this)))

;; Tss (npm i -g typescript-tools)
(leaf tss
  :ensure t
  :config
  (setq tss-popup-help-key "C-:")
  (setq tss-jump-to-definition-key "C->")
  (setq tss-implement-definition-key "C-c i")
  (add-hook 'typescript-mode-hook #'tss-setup-current-buffer)
  ;; (add-hook 'kill-buffer-hook 'tss--delete-process t)
  )

;; Flycheck specifics
(leaf flycheck-typescript-tslint
  :el-get Simplify/flycheck-typescript-tslint
  :emacs> 23
  :after flycheck
  :config
  (add-hook 'typescript-mode-hook #'flycheck-typescript-tslint-setup)
  (defun sanityinc/flycheck-typescript-reconfigure ()
    "Reconfigure flycheck typescript settings, e.g. after changing cabal file."
    (interactive)
    (unless (eq major-mode 'typescript-mode)
      (error "Expected to be in typescript-mode"))
    ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (company-mode +1)
    (flycheck-typescript-clear-config-cache)
    (flycheck-typescript-configure)
    (flycheck-mode -1)
    (flycheck-mode))
  (defadvice typescript-mode-stylish-buffer (around skip-if-flycheck-errors activate)
    "Don't run stylish-buffer if the buffer appears to have a syntax error.
This isn't a hard guarantee, since flycheck might sometimes not run until the file has
been saved."
    (unless (flycheck-has-current-errors-p 'error)
      ad-do-it))
  (require 'flycheck-typescript-tslint))


;;; Run and interact with an inferior JS via js-comint.el
(setq inferior-js-program-command "js")

(defvar inferior-js-minor-mode-map (make-sparse-keymap))
(define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
(define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
(define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
(define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
(define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

(define-minor-mode inferior-js-keys-mode
  "Bindings for communicating with an inferior js interpreter."
  nil " InfJS" inferior-js-minor-mode-map)

(dolist (hook '(js2-mode-hook js-mode-hook))
  (add-hook hook 'inferior-js-keys-mode))


;;; Alternatively, use skewer-mode
(leaf skewer-mode
  :ensure t
  :emacs>= 24
  :if (featurep 'js2-mode)
  :after skewer-mode
  :config
  (add-hook 'skewer-mode-hook
            (lambda () (inferior-js-keys-mode -1))))


;;; JSDoc
(leaf js-doc
  :ensure t
  :config
  (setq js-doc-mail-address "your email address"
        js-doc-author (format "your name <%s>" js-doc-mail-address)
        js-doc-url "url of your website"
        js-doc-license "license name")
  (add-hook 'js-mode-hook
            #'(lambda ()
                (define-key js-mode-map (kbd "C-c i") 'js-doc-insert-function-doc)
                ;; (define-key js-mode-map (kbd "@") 'js-doc-insert-tag)
                )))


;;; React
(leaf rjsx-mode
  :ensure t
  :config
  (with-eval-after-load 'rjsx-mode
    (define-key rjsx-mode-map "<" nil)
    (define-key rjsx-mode-map (kbd "C-d") nil)
    (define-key rjsx-mode-map ">" nil)
    (define-key rjsx-mode-map "C" nil)
    (define-key rjsx-mode-map (kbd "C-c / .") 'sgml-tag)))



(provide 'init-javascript)
;;; init-javascript.el ends here
