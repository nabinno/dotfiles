;;; init-html --- initial html-mode configuration
;;; Commentary:
;;; Code:
(use-package tidy
  :straight (:host github :repo "lwiechec/tidy.el"))

(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))


(use-package tagedit :straight t)
(after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.(jsp|tmpl)\\'")

;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)
;;; init-html ends here
