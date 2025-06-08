;;; init-crontab --- configuration of crontab-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf crontab-mode :ensure t)
(add-auto-mode 'crontab-mode "\\.?cron\\(tab\\)?\\'")



(provide 'init-crontab)
;;; init-crontab.el ends here
