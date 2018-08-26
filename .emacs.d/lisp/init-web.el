;;; init-web --- initial web-mode configuration
;;; Commentary:
;;; Code:
(unless (require 'web-mode nil 'noerror)
  (el-get-bundle fxbois/web-mode))

(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; coloration
(custom-set-faces
 '(web-mode-comment-face           ((t (:foreground "green"))))
 '(web-mode-css-selector-face      ((t (:foreground "blue" :weight bold))))
 '(web-mode-css-property-name-face ((t (:foreground "yellow"))))
 '(web-mode-doctype-face           ((t (:foreground "green"))))
 '(web-mode-html-attr-name-face    ((t (:foreground "purple"))))
 '(web-mode-html-attr-value-face   ((t (:foreground "white"))))
 '(web-mode-html-tag-face          ((t (:foreground "blue" :weight bold))))
 '(web-mode-server-comment-face    ((t (:foreground "green"))))
 )

(defun web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-engines-alist '(("php" . "\\.ctp\\'")
                                 )))
(add-hook 'web-mode-hook  'web-mode-hook)


;; prettier
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.
MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))
(add-hook 'web-mode-hook #'(lambda () (enable-minor-mode '("\\.jsx?\\'" . prettier-js-mode))))
(add-hook 'web-mode-hook #'(lambda () (enable-minor-mode '("\\.eex\\'" . prettier-js-mode))))


(provide 'init-web)
;;; init-web.el ends here
