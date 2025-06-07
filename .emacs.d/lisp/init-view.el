;;; init-view --- Configure view mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq view-read-only t)
(global-set-key (kbd "") 'view-mode)


;;; Pager keybind
(defvar pager-keybind
  '(("h" . backward-char)
    ("l" . forward-char)
    ("j" . next-line)
    ("k" . previous-line)
    ("x" . kill-this-buffer)
    (";" . kill-this-buffer)
    ("u" . scroll-down)
    ("b" . scroll-up)
    (" " . scroll-up)
    ;; ("g" . beginning-of-buffer)
    ("p" . beginning-of-buffer)
    ;; ("e" . end-of-buffer)
    ("n" . end-of-buffer)
    ;; ("J" . forward-list)
    ;; ("K" . backward-list)
    ("y" . origami-cycle-universally)
    ("o" . View-quit)
    ("i" . View-quit)
    ;; ("" . View-quit)
    ))
(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)
(defun view-mode-hook0 ()
  "Wrap view-mode-hook."
  (define-many-keys view-mode-map pager-keybind)
  ;; (hl-line-mode 1)
  (define-key view-mode-map " " 'scroll-up))
(add-hook 'view-mode-hook 'view-mode-hook0)
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))
(defvar view-mode-force-exit nil)
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not view-mode-force-exit)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))
(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)



(provide 'init-view)
;;; init-view.el ends here
