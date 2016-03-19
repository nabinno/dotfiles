;;; init-w3m --- w3m configuraiton
;;; Commentary:
;;; Code:
(require-package 'w3m)

(setq browse-url-browser-function 'w3m-goto-url
      browse-url-generic-program "")


(provide 'init-w3m)
;;; init-w3m.el ends here.
