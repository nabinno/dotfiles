;;; init-restclient -- restclient configuration
;;; Commentary:
;;; Code:
(require-package 'restclient)
(require-package 'restclient-helm)
;; (require-package 'company-restclient)

(require-package 'ob-restclient)
(org-babel-do-load-languages
 'org-babel-load-languages
  '((restclient . t)))



(provide 'init-restclient)
;;; init-restclient.el ends here
