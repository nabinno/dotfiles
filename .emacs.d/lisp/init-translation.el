;;; init-translation --- basic translation configuration
;;; Commentary:
;;; Code:
(leaf google-translate
  :ensure t
  :init
  (require 'google-translate-default-ui)
  :config
  (global-set-key "\C-ct" 'google-translate-at-point)
  (global-set-key "\C-cT" 'google-translate-query-translate)

  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja")))


;;; popwin.el
(leaf popwin
  :ensure t
  :config
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-position 'bottom)
  (push '("*Google Translate*") popwin:special-display-config))


;;; google
(leaf google-this
  :ensure t
  :config
  (google-this-mode 1))


;;; eijiro
(leaf search-web :ensure t)


;;; codic
(leaf codic :ensure t)


(provide 'init-translation)
;;; init-translation.el ends here
