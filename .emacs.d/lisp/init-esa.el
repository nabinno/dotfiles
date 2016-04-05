;;; init-esa --- esa configuration
;;; Commentary:
;;; Code:
(unless (require 'esa nil 'noerror)
    (el-get-bundle nabinno/esa.el))

(global-set-key (kbd "C-c ; e l") 'esa-list)
(global-set-key (kbd "C-c ; e b") 'esa-buffer-wip)
(global-set-key (kbd "C-c ; e r") 'esa-region-wip)


(provide 'init-esa)
;;; init-esa.el ends here
