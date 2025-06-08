;;; init-windows --- windows configuraiton -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
;;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(winner-mode 1)

;; (global-set-key (kbd "M-[ 1 ; 6 B") 'balance-windows)


;;; Make "C-x o" prompt for a target window when there are more than 2
(leaf switch-window
  :ensure t
  :config
  (setq switch-window-shortcut-style 'alphabet)
  (global-set-key (kbd "C-x o") 'switch-window))

;; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))
(global-set-key "\C-x1" 'sanityinc/toggle-delete-other-windows)

;; Rearrange split windows
(defun split-window-horizontally-instead (num_wins)
  (interactive "p")
  (if (> num_wins 2)
      (progn (delete-other-windows)
             (dotimes (n (- num_wins 1)) (split-window-horizontally))
             (balance-windows))
    (progn (delete-other-windows)
           (split-window-horizontally))))
(defun split-window-vertically-instead (num_wins)
  (interactive "p")
  (if (> num_wins 2)
      (progn (delete-other-windows)
             (dotimes (n (- num_wins 1)) (split-window-vertically))
             (balance-windows))
    (progn (delete-other-windows)
           (split-window-vertically))))
(defun split-window-crossly-instead ()
  (interactive)
  (progn
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-vertically)
    (other-window 2)
    (split-window-vertically)
    (balance-windows)))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)
(global-set-key "\C-x+" 'split-window-crossly-instead)


;;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (switch-to-buffer nil)))


;;; Other window or split
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(defun split-window-vertically-x ()
  (interactive)
  (split-window-vertically
   (- (window-height) (/ (window-height) 4))))
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (< (window-body-width) 110)
        (progn
          (split-window-vertically-x)
          (sr-speedbar-toggle)
          (other-window 1)
          (multi-eshell--kill-all)
          (multi-eshell 1))
      (if (>= (window-body-width) 200)
          (progn
            (sr-speedbar-toggle)
            (split-window-vertically-x)
            (split-window-horizontally-n 3)
            (other-window 4)
            (multi-eshell--kill-all)
            (multi-eshell 1))
        (split-window-horizontally))))
  (other-window 1))
(defun delete-other-windows-and-speedbar-close ()
  (interactive)
  (progn
    (delete-other-windows)
    (sr-speedbar-close)))

;; (global-set-key (kbd "M-[ 1 ; 5 i") 'other-window-or-split)
(global-set-key (kbd "<backtab>") 'delete-other-windows-and-speedbar-close)


(provide 'init-windows)
;;; init-windows.el ends here
