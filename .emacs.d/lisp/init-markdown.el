;;; init-markdown --- Markdown configuration file -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf markdown-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (cons '("\\.\\(md\\|markdown\\|apib\\)\\'" . markdown-mode) auto-mode-alist))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (define-key markdown-mode-map (kbd "M-e") 'markdown-cycle)
              (define-key markdown-mode-map (kbd "C-c C-c") 'restclient-http-send-current)
              (define-key markdown-mode-map (kbd "C-c C-v") 'restclient-http-send-current-stay-in-window)
              ;; (hide-sublevels 2)
              )))


;;; Hyde (Jekyll client)
(leaf adaptive-wrap :ensure t)
(leaf hyde
  :ensure t
  :config
  (setq hyde-home "~/nabinno.github.io")
  (defun hyde/open-post-maybe-into-other-window (pos)
    "Opens the post under cursor in the editor (POS)."
    (interactive "d")
    (let ((post-file-name (nth
                           1
                           (split-string (strip-string (thing-at-point 'line)) " : ")))
          (dir (get-text-property pos 'dir)))
      (let ((hyde-buffer (current-buffer)))
        (find-file-other-window
         (strip-string (concat hyde-home "/" dir "/" post-file-name)))
        (hyde-markdown-activate-mode hyde-buffer)
        (adaptive-wrap-prefix-mode t)
        (set-default 'truncate-lines nil))))
  (defun hyde/quit-wrap ()
    "Quits hyde."
    (interactive)
    (progn
      (delete-other-windows)
      (kill-buffer (current-buffer))))
  (defun create-markdown-scratch ()
    "Create a markdown scratch buffer."
    (interactive)
    (switch-to-buffer (get-buffer-create "*markdown*"))
    (markdown-mode))
  (defun hyde/nabinno ()
    "Run hyde-wrap with home parameter."
    (interactive)
    (progn
      (delete-other-windows)
      (create-markdown-scratch)
      (split-window-horizontally)
      (other-window 1)
      (hyde "~/nabinno.github.io/")))
  (defvar hyde-mode-map
    (let
        ((hyde-mode-map (make-sparse-keymap)))
      (define-key hyde-mode-map (kbd "N") 'hyde/new-post)
      (define-key hyde-mode-map (kbd "G") 'hyde/load-posts)
      (define-key hyde-mode-map (kbd "C") 'hyde/hyde-commit-post)
      (define-key hyde-mode-map (kbd "P") 'hyde/hyde-push)
      (define-key hyde-mode-map (kbd "J") 'hyde/run-jekyll)
      (define-key hyde-mode-map (kbd "S") 'hyde/serve)
      (define-key hyde-mode-map (kbd "K") 'hyde/stop-serve)
      (define-key hyde-mode-map (kbd "d") 'hyde/deploy)
      (define-key hyde-mode-map (kbd "D") 'hyde/delete-post)
      (define-key hyde-mode-map (kbd "U") 'hyde/promote-to-post)
      (define-key hyde-mode-map (kbd "X") 'hyde/quit-wrap)
      (define-key hyde-mode-map (kbd "O") 'hyde/open-post-maybe-into-other-window)
      hyde-mode-map)
    "Keymap for Hyde")
  (global-set-key (kbd "C-c ; j") 'hyde/nabinno))


(provide 'init-markdown)
;;; init-markdown.el ends here
