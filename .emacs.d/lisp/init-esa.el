;;; init-esa --- esa configuration
;;; Commentary:
;;; Code:
(unless (require 'esa nil 'noerror)
    (el-get-bundle nabinno/esa.el))

(global-set-key (kbd "C-c ; e") 'esa-list)
(global-set-key (kbd "C-c ; E") 'esa-buffer-wip)


(provide 'init-esa)
;;; init-esa.el ends here
