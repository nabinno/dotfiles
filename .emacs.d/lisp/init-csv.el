(use-package csv-mode :straight t)
(use-package csv-nav
  :straight (:host github :repo "emacsmirror/csv-nav"))

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
