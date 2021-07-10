;;; init-crontab --- configuration of crontab-mode
;;; Commentary:
;;; Code:
(use-package crontab-mode :straight t)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")



(provide 'init-crontab)
;;; init-crontab ends here
