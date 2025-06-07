;;; init-css --- Colourise CSS colour literals -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf rainbow-mode
  :ensure t
  :emacs>= 24
  :config
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))


;; ;;; Embedding in html
;; (leaf mmm-mode :ensure t)
;; (after-load 'mmm-vars
;;   (mmm-add-group
;;    'html-css
;;    '((css-cdata
;;       :submode css-mode
;;       :face mmm-code-submode-face
;;       :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
;;       :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
;;       :insert ((?j js-tag nil @ "<style type=\"text/css\">"
;;                    @ "\n" _ "\n" @ "</script>" @)))
;;      (css
;;       :submode css-mode
;;       :face mmm-code-submode-face
;;       :front "<style[^>]*>[ \t]*\n?"
;;       :back "[ \t]*</style>"
;;       :insert ((?j js-tag nil @ "<style type=\"text/css\">"
;;                    @ "\n" _ "\n" @ "</style>" @)))
;;      (css-inline
;;       :submode css-mode
;;       :face mmm-code-submode-face
;;       :front "style=\""
;;       :back "\"")))
;;   (dolist (mode (list 'html-mode 'nxml-mode))
;;     (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))


;;; SASS and SCSS
(leaf sass-mode :ensure t)
(leaf scss-mode
  :ensure t
  :config
  (setq-default scss-compile-at-save nil)
  (defun scss-custom ()
    "scss-mode-hook"
    (and
     (set (make-local-variable 'css-indent-offset) 2)
     (set (make-local-variable 'scss-compile-at-save) nil)))
  (add-hook 'scss-mode-hook '(lambda() (scss-custom))))


;;; LESS
(leaf less-css-mode :ensure t)
(leaf skewer-less
  :ensure t
  :if (featurep 'js2-mode))


;;; Auto-complete CSS keywords
(after-load 'auto-complete
  (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
    (add-hook hook 'ac-css-mode-setup)))


;;; Use eldoc for syntax hints
(leaf css-eldoc
  :ensure t
  :config
  (autoload 'turn-on-css-eldoc "css-eldoc")
  (add-hook 'css-mode-hook 'turn-on-css-eldoc))


(provide 'init-css)
;;; init-css.el ends here
