;;; init-translation --- basic translation configuration
;;; Commentary:
;;; Code:
(require-package 'google-translate)
(require 'google-translate-default-ui)

(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

(custom-set-variables
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja"))


;;; popwin.el
(require-package 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(push '("*Google Translate*") popwin:special-display-config)


;;; google
(require-package 'google-this)
(google-this-mode 1)


;;; eijiro
(require-package 'search-web)


;;; codic
(require-package 'codic)


(provide 'init-translation)
;;; init-translation.el ends here
