;;; init-restclient -- restclient configuration
;;; Commentary:
;;; Code:
(use-package restclient :straight t)
(use-package restclient-helm :straight t)
;; (use-package company-restclient :straight t)

(use-package ob-restclient :straight t)
(org-babel-do-load-languages
 'org-babel-load-languages
  '((restclient . t)))



(provide 'init-restclient)
;;; init-restclient.el ends here
