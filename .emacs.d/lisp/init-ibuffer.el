;;; init-ibuffer --- ibuffer configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf fullframe :ensure t)
(after-load 'ibuffer
 (fullframe ibuffer ibuffer-quit))
(leaf ibuffer-vc :ensure t)

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))
(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

;; Explicitly require ibuffer-vc to get its column definitions, which
;; can't be autoloaded
(after-load 'ibuffer (require 'ibuffer-vc))

;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))
(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

;; Keybind
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-SPC") 'ibuffer)
;; (global-set-key (kbd "C-x e") 'electric-buffer-list)

(eval-after-load "ibuffer"
  '(progn
     (add-hook 'ibuffer-mode-hook
               (lambda ()
                 (define-key ibuffer-mode-map "j"    'ibuffer-forward-line)
                 (define-key ibuffer-mode-map "k"    'ibuffer-backward-line)
                 (define-key ibuffer-mode-map " "    'ibuffer-mark-forward)
                 (define-key ibuffer-mode-map "\M-a" '(lambda () (interactive) (ibuffer-unmark-all 1)))
                 ))))



(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
