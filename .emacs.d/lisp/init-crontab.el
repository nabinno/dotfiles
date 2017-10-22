;;; init-crontab --- configuration of crontab-mode
;;; Commentary:
;;; Code:

(require-package 'crontab-mode)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")



(provide 'init-crontab)
;;; init-crontab ends here
