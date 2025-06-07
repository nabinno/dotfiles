;;; init-grep -- grep configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Ag / The Silver Searcher
(leaf ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window 'nil)
  (setq ag-reuse-buffers 'nil))

(defun ag-visit-buffer-other-window (&optional event noselect)
  "Visit ag result in another window with EVENT and NOSELECT."
  (interactive)
  (let ((current-window (selected-window)))
    ;; ag-mode uses compile-goto-error for visiting the result
    (compile-goto-error event)
    (when noselect
      (select-window current-window))))
(defun ag-visit-buffer-other-window-noselect (&optional event)
  "Visit ag result in another window with EVENT, but don't select it."
  (interactive)
  (ag-visit-buffer-other-window event t))
(fset 'ag-visit-buffer-other-window-noselect-and-next-error "xcompilation-next-errorxag-visit-buffer-other-window-noselect")

;; keybind
(add-hook 'ag-mode-hook
          (lambda ()
            ;; make C-o and o behave as in dired
            (define-key ag-mode-map (kbd "h") 'ag-visit-buffer-other-window-noselect)
            (define-key ag-mode-map (kbd "m") 'ag-visit-buffer-other-window-noselect-and-next-error)
            (define-key ag-mode-map (kbd "o") 'ag-visit-buffer-other-window)
            (define-key ag-mode-map (kbd "J") 'next-error-no-select)
            (define-key ag-mode-map (kbd "K") 'previous-error-no-select)
            (define-key ag-mode-map (kbd "j") 'compilation-next-error)
            (define-key ag-mode-map (kbd "k") 'compilation-previous-error)
            (define-key ag-mode-map (kbd "n") 'compilation-next-file)
            (define-key ag-mode-map (kbd "p") 'compilation-previous-file)
            (define-key ag-mode-map (kbd "b") 'scroll-up-command)
            (define-key ag-mode-map (kbd "u") 'scroll-down-command)
            ))

;; wgrep
(add-hook 'ag-mode-hook '(lambda ()
                           (unless (require 'wgrep-ag nil 'noerror)
                             (el-get-bundle mhayashi1120/Emacs-wgrep))
                           (setq wgrep-auto-save-buffer t)
                           (setq wgrep-enable-key "r")
                           (wgrep-ag-setup)))


;;; Grep
;; keybind
(add-hook 'grep-mode-hook
          (lambda ()
           (define-key grep-mode-map (kbd "J") 'next-error-no-select)
           (define-key grep-mode-map (kbd "K") 'previous-error-no-select)
           (define-key grep-mode-map (kbd "j") 'compilation-next-error)
           (define-key grep-mode-map (kbd "k") 'compilation-previous-error)
           (define-key grep-mode-map (kbd "n") 'compilation-next-file)
           (define-key grep-mode-map (kbd "p") 'compilation-previous-file)
           (define-key grep-mode-map (kbd "b") 'scroll-up-command)
           (define-key grep-mode-map (kbd "u") 'scroll-down-command)
           ))


(provide 'init-grep)
;;; init-grep.el ends here
