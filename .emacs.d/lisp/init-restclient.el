;;; init-restclient -- restclient configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf restclient :ensure t)
(leaf restclient-helm :ensure t)
;; (leaf company-restclient :ensure t)

(leaf ob-restclient :ensure t)
(org-babel-do-load-languages
 'org-babel-load-languages
  '((restclient . t)))



(provide 'init-restclient)
;;; init-restclient.el ends here
