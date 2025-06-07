;;; init-csv --- CSV configuraiton -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf csv-mode :ensure t)
(leaf csv-nav
  :el-get emacsmirror/csv-nav)

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))


(provide 'init-csv)
;;; init-csv.el ends here
