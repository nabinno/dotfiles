;;; init-sql -- sql configuration
;;;
;;; Commentary:
;;;   C-c C-c : 'sql-send-paragraph
;;;   C-c C-r : 'sql-send-region
;;;   C-c C-s : 'sql-send-string
;;;   C-c C-b : 'sql-send-buffer
;;;
;;; Code:
(leaf sql-indent :ensure t)
(after-load 'sql
  (require 'sql-indent))

(defun sanityinc/pop-to-sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (if sql-buffer
      (progn
        (pop-to-buffer sql-buffer)
        (goto-char (point-max)))
    (sql-set-sqli-buffer)
    (when sql-buffer
      (sanityinc/pop-to-sqli-buffer))))

(after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)
  (define-key sql-mode-map (kbd "C-M-n") 'comint-previous-matching-input-from-input)
  (define-key sql-mode-map (kbd "C-M-p") 'comint-next-matching-input-from-input)
  (add-hook 'sql-interactive-mode-hook 'sanityinc/never-indent)
  (when (package-installed-p 'dash-at-point)
    (defun sanityinc/maybe-set-dash-db-docset ()
      (when (eq sql-product 'postgres)
        (setq dash-at-point-docset "psql")))
    (add-hook 'sql-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (add-hook 'sql-interactive-mode-hook 'sanityinc/maybe-set-dash-db-docset)
    (defadvice sql-set-product (after set-dash-docset activate)
      (sanityinc/maybe-set-dash-db-docset)))
  )

(setq-default sql-input-ring-file-name
              (expand-file-name ".sqli_history" user-emacs-directory))

(after-load 'page-break-lines
  (push 'sql-mode page-break-lines-modes))


;;; Redis
(leaf redis :ensure t)


(provide 'init-sql)
;;; init-sql.el ends here
