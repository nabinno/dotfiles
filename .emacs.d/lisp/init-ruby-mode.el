;;; init-ruby-mode -- basic ruby configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf ruby-mode :ensure t)
(leaf ruby-hash-syntax :ensure t)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")

(setq ruby-use-encoding-map nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)

  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(add-hook 'ruby-mode-hook 'subword-mode)

(defun ruby-mode-set-encoding () ())

;; TODO: hippie-expand ignoring : for names in ruby-mode
;; TODO: hippie-expand adaptor for auto-complete sources


;;; Rufo
(leaf rufo
  :el-get danielma/rufo.el
  :config
  (add-hook 'ruby-mode-hook 'rufo-minor-mode))


;;; Inferior ruby
(leaf inf-ruby
  :ensure t
  :config
  ;; (leaf ac-inf-ruby :ensure t)
  ;; (after-load 'auto-complete
  ;;   (add-to-list 'ac-modes 'inf-ruby-mode))
  ;; (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
  ;; (after-load 'inf-ruby
  ;;   (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))
  )


;;; Ruby compilation
(leaf ruby-compilation
  :ensure t
  :config
  (after-load 'ruby-mode
    (let ((m ruby-mode-map))
      (define-key m [S-f7] 'ruby-compilation-this-buffer)
      (define-key m [f7] 'ruby-compilation-this-test)
      (define-key m [f6] 'recompile))))


;; ;;; Robe
;; (leaf robe :ensure t)
;; (after-load 'ruby-mode
;;   (add-hook 'ruby-mode-hook 'robe-mode))
;; (after-load 'robe
;;   (add-hook 'robe-mode-hook
;;             (lambda ()
;;               (add-to-list 'ac-sources 'ac-source-robe)
;;               (set-auto-complete-as-completion-at-point-function))))


;;; ri support
(leaf yari :ensure t)
(defalias 'ri 'yari)


;;; YAML
(leaf yaml-mode :ensure t)


;;; Slim
(leaf slim-mode :ensure t)


;; ;;; ERB
;; (leaf mmm-mode :ensure t)
;; (defun sanityinc/ensure-mmm-erb-loaded ()
;;   (require 'mmm-erb))
;;
;; (require 'derived)
;;
;; (defun sanityinc/set-up-mode-for-erb (mode)
;;   (add-hook (derived-mode-hook-name mode) 'sanityinc/ensure-mmm-erb-loaded)
;;   (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))
;;
;; (let ((html-erb-modes '(html-mode html-erb-mode nxml-mode)))
;;   (dolist (mode html-erb-modes)
;;     (sanityinc/set-up-mode-for-erb mode)
;;     (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
;;     (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))
;;
;; (mapc 'sanityinc/set-up-mode-for-erb
;;       '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))
;;
;; (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
;;
;; (add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
;; (add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))
;; (mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\'" 'erb)
;;
;; (dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
;;   (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))


;;; My convention for heredocs containing SQL

;; Needs to run after rinari to avoid clobbering font-lock-keywords?
;; (leaf mmm-mode :ensure t)
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))
;; (add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)


;;; Encoding
(defun ruby-mode-hook-init ()
  "Disable auto-insert-encoding"
  (remove-hook 'before-save-hook 'ruby-mode-set-encoding))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-init)
(defun my-ruby-mode-set-encoding ()
  "set-encoding ruby-mode"
  (interactive)
  (ruby-mode-set-encoding))
(define-key ruby-mode-map "\C-ce" 'my-ruby-mode-set-encoding)


(provide 'init-ruby-mode)
;;; init-ruby-mode.el ends here
