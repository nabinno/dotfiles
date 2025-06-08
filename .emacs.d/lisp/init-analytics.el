;;; init-analytics.el -- analytics configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf es-mode :ensure t) ;; Elasticsearch


(provide 'init-analytics)
;;; init-analytics.el ends here
