;;; init-translation --- basic translation configuration
;;; Commentary:
;;; Code:
(require-package 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)


(provide 'init-translation)
;;; init-translation.el ends here
