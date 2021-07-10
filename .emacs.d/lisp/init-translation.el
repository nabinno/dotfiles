;;; init-translation --- basic translation configuration
;;; Commentary:
;;; Code:
(use-package google-translate :straight t)
(require 'google-translate-default-ui)

(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

(custom-set-variables
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja"))


;;; popwin.el
(use-package popwin :straight t)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(push '("*Google Translate*") popwin:special-display-config)


;;; google
(use-package google-this :straight t)
(google-this-mode 1)


;;; eijiro
(use-package search-web :straight t)


;;; codic
(use-package codic :straight t)


(provide 'init-translation)
;;; init-translation.el ends here
