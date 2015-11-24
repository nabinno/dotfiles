;;; init-dired --- dired configuration
;;; Commentary:
;;; Code:
(require-package 'dired+)

(setq diredp-hide-details-initially-flag nil)
(setq dired-listing-switches "-AFl --group-directories-first")
(setq dired-recursive-deletes 'top)
(put 'dired-find-alternate-file 'disabled nil)

(defun dired-find-file-x (&optional arg)
  "Open each of the marked files, or the file under the point, or
when prefix arg, the next N files "
  (interactive "P")
  (let* ((fn-list (dired-get-marked-files nil arg)))
    (mapc 'find-file fn-list)))
(defun dired-find-file-other-window-noselect ()
  "Open the marked file on other window without select."
  (interactive)
  (progn (dired-find-file-other-window) (other-window 1)))
(defun dired-copy-to-other-window (arg)
  "In dired, copy selected file(s) to the other window."
  (interactive "P")
  (let ((dired-dwim-target t))
    (dired-do-copy arg)))
(defun dired-move-to-other-window (arg)
  "In dired, rename selected file(s) to the other window."
  (interactive "P")
  (let ((dired-dwim-target t)
        (use-file-dialog  nil))
    (dired-do-rename arg)))
(defun dired-make-symlinks-to-other-window (arg)
  "In dired, make symbolic link(s) of selected file(s) to the
other window."
  (interactive "P")
  (let ((dired-dwim-target t)
        (use-file-dialog  nil))
    (dired-do-symlink arg)))
(defun dired-change-other-window-to-same-directory ()
  "In dired, change other window directory to current window."
  (interactive)
  (let ((current-buffer-name (buffer-name (current-buffer))))
    (progn (delete-other-windows)
           (split-window-horizontally)
           (other-window 1)  (switch-to-buffer current-buffer-name)
           (other-window 1))))
(defun dired-change-current-window-to-oposite-directory ()
  "In dired, change current window directory to other window."
  (interactive)
  (let ((other-buffer-name
         (buffer-name
          (progn
            (other-window 1) (current-buffer)))))
    (progn (delete-other-windows)
           (split-window-horizontally)
           (switch-to-buffer other-buffer-name)
           (other-window 1)  (switch-to-buffer other-buffer-name))))
(defun dired-beginning-of-lines ()
  "In dired, move cussor to end of lines"
  (interactive)
  (progn
    (beginning-of-buffer) (diredp-next-line 2)))
(defun dired-end-of-lines ()
  "In dired, move cussor to end of lines"
  (interactive)
  (progn (end-of-buffer) (diredp-previous-line 1)))
(defun dired-mark-all ()
  "In dired, mark all"
  (interactive)
  (progn (dired-unmark-all-marks) (dired-toggle-marks)))
(defun dired-next-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
         (not (string-match-p "dired-mode" (message "%s" major-mode)))
         (not (equal bread-crumb (buffer-name))))
      (next-buffer))))
(defun dired-previous-buffer ()
  (interactive)
  (let ((bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (not (string-match-p "dired-mode" (message "%s" major-mode)))
         (not (equal bread-crumb (buffer-name))))
      (previous-buffer))))
(defun dired-search-word (&optional arg)
  "In dired, search word."
  (interactive "P")
  (let ((word (concat "[0-9]\\{2\\}:[0-9]\\{2\\} " arg)))
    (progn (search-forward-regexp word))))

;; keybind
(eval-after-load "dired"
  '(progn
     (require 'dired+)
     (when (fboundp 'global-dired-hide-details-mode)
       (global-dired-hide-details-mode -1))
     (add-hook 'dired-mode-hook
               (lambda ()
                 (guide-key/add-local-guide-key-sequence "%")
                 (define-key dired-mode-map "m"    'dired-move-to-other-window)
                 (define-key dired-mode-map "c"    'dired-copy-to-other-window)
                 (define-key dired-mode-map "s"    'dired-make-symlinks-to-other-window)
                 (define-key dired-mode-map "j"    'diredp-next-line)
                 (define-key dired-mode-map "k"    'diredp-previous-line)
                 (define-key dired-mode-map "p"    'dired-beginning-of-lines)
                 (define-key dired-mode-map "n"    'dired-end-of-lines)
                 (define-key dired-mode-map "K"    'dired-create-directory)
                 (define-key dired-mode-map "e"    'dired-find-file-x)
                 (define-key dired-mode-map "E"    'dired-find-file-other-window-noselect)
                 (define-key dired-mode-map "h"    'dired-find-file-other-window-noselect)
                 (define-key dired-mode-map "A"    'dired-mark-all)
                 (define-key dired-mode-map "\M-a" 'dired-unmark-all-marks)
                 (define-key dired-mode-map "o"    'dired-view-file)
                 (define-key dired-mode-map "O"    'dired-change-other-window-to-same-directory)
                 (define-key dired-mode-map "" 'dired-change-current-window-to-oposite-directory)
                 (define-key dired-mode-map ""   'dired-up-directory)
                 (define-key dired-mode-map "i"    'dired-up-directory)
                 (define-key dired-mode-map "r"    'wdired-change-to-wdired-mode)
                 (define-key dired-mode-map ""   'dired-view-file)
                 (define-key dired-mode-map " "    'dired-mark)
                 (define-key dired-mode-map "f"    'isearch-forward)
                 (define-key dired-mode-map "	"  'switch-window)
                 (define-key dired-mode-map "\C-a" '(lambda () (interactive) (dired-search-word "a")))
                 (define-key dired-mode-map "\C-b" '(lambda () (interactive) (dired-search-word "b")))
                 (define-key dired-mode-map "\C-c" '(lambda () (interactive) (dired-search-word "c")))
                 (define-key dired-mode-map "\C-d" '(lambda () (interactive) (dired-search-word "d")))
                 (define-key dired-mode-map "\C-e" '(lambda () (interactive) (dired-search-word "e")))
                 (define-key dired-mode-map "\C-f" '(lambda () (interactive) (dired-search-word "f")))
                 (define-key dired-mode-map "\C-g" '(lambda () (interactive) (dired-search-word "g")))
                 (define-key dired-mode-map "\C-h" '(lambda () (interactive) (dired-search-word "h")))
                 (define-key dired-mode-map "\C-j" '(lambda () (interactive) (dired-search-word "j")))
                 (define-key dired-mode-map "\C-k" '(lambda () (interactive) (dired-search-word "k")))
                 (define-key dired-mode-map "\C-l" '(lambda () (interactive) (dired-search-word "l")))
                 (define-key dired-mode-map "\C-m" '(lambda () (interactive) (dired-search-word "m")))
                 (define-key dired-mode-map "\C-n" '(lambda () (interactive) (dired-search-word "n")))
                 (define-key dired-mode-map "\C-o" '(lambda () (interactive) (dired-search-word "o")))
                 (define-key dired-mode-map "\C-p" '(lambda () (interactive) (dired-search-word "p")))
                 (define-key dired-mode-map "\C-q" '(lambda () (interactive) (dired-search-word "q")))
                 (define-key dired-mode-map "\C-r" '(lambda () (interactive) (dired-search-word "r")))
                 (define-key dired-mode-map "\C-s" '(lambda () (interactive) (dired-search-word "s")))
                 (define-key dired-mode-map "\C-t" '(lambda () (interactive) (dired-search-word "t")))
                 (define-key dired-mode-map "\C-u" '(lambda () (interactive) (dired-search-word "u")))
                 (define-key dired-mode-map "\C-v" '(lambda () (interactive) (dired-search-word "v")))
                 (define-key dired-mode-map "\C-w" '(lambda () (interactive) (dired-search-word "w")))
                 (define-key dired-mode-map "\C-x" '(lambda () (interactive) (dired-search-word "x")))
                 (define-key dired-mode-map "\C-y" '(lambda () (interactive) (dired-search-word "y")))
                 (define-key dired-mode-map "\C-z" '(lambda () (interactive) (dired-search-word "z")))
                 (define-key dired-mode-map (kbd "M-[ 1 ; 3 C") 'dired-next-buffer)
                 (define-key dired-mode-map (kbd "M-[ 1 ; 3 D") 'dired-previous-buffer)
                 ))))


;;; ls for darwin
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))



(provide 'init-dired)
;;; init-dired.el ends here.
