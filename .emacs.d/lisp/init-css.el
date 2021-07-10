;;; init-css --- Colourise CSS colour literals
;;; Commentary:
;;; Code:
(when (eval-when-compile (>= emacs-major-version 24))
  ;; rainbow-mode needs color.el, bundled with Emacs >= 24.
  (use-package rainbow-mode :straight t)
  (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
    (add-hook hook 'rainbow-mode)))


;; ;;; Embedding in html
;; (use-package mmm-mode :straight t)
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
(use-package sass-mode :straight t)
(use-package scss-mode :straight t)
(setq-default scss-compile-at-save nil)
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)))
(add-hook 'scss-mode-hook '(lambda() (scss-custom)))


;;; LESS
(use-package less-css-mode :straight t)
(when (featurep 'js2-mode)
  (use-package skewer-less :straight t))



;;; Auto-complete CSS keywords
(after-load 'auto-complete
  (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
    (add-hook hook 'ac-css-mode-setup)))


;;; Use eldoc for syntax hints
(use-package css-eldoc :straight t)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)


(provide 'init-css)
;;; init-css.el ends here
