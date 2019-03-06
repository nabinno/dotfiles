(require-package 'csv-mode)
(unless (require 'csv-nav nil 'noerror)
  (el-get-bundle emacsmirror/csv-nav))

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
