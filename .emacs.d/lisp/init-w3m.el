;;; init-w3m --- w3m configuraiton
;;; Commentary:
;;; Code:
(use-package w3m :straight t)

(setq browse-url-browser-function 'w3m-goto-url
      browse-url-generic-program "")


(provide 'init-w3m)
;;; init-w3m.el ends here.
