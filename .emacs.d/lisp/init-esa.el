;;; init-esa --- esa configuration
;;; Commentary:
;;; Code:
(require-package 'esa)

(global-set-key (kbd "C-c ; e l") 'esa-list)
(global-set-key (kbd "C-c ; e b") 'esa-buffer-wip)
(global-set-key (kbd "C-c ; e r") 'esa-region-wip)

(add-hook 'esa-describe-write-mode-hook
          (lambda ()
            (define-key esa-describe-write-mode-map (kbd "C-c C-v") 'restclient-http-send-current-stay-in-window)))


(provide 'init-esa)
;;; init-esa.el ends here
