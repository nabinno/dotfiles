;;; init-restclient -- restclient configuration
;;; Commentary:
;;; Code:
(leaf restclient :ensure t)
(leaf restclient-helm :ensure t)
;; (leaf company-restclient :ensure t)

(leaf ob-restclient :ensure t)
(org-babel-do-load-languages
 'org-babel-load-languages
  '((restclient . t)))



(provide 'init-restclient)
;;; init-restclient.el ends here
