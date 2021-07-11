;;; init-crontab --- configuration of crontab-mode
;;; Commentary:
;;; Code:
(leaf crontab-mode :ensure t)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")



(provide 'init-crontab)
;;; init-crontab ends here
