;;; init-ido --- ido configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Use C-f during file selection to switch to regular find-file
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

;; (when (eval-when-compile (>= emacs-major-version 24))
;;  (require-package 'ido-ubiquitous)
;;  (ido-ubiquitous-mode t))

;; Use smex to handle M-x
(when (eval-when-compile (>= emacs-major-version 24))
  (leaf smex :ensure t)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(leaf idomenu :ensure t)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))

(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                           "*Messages*" "Async Shell Command"))


;;; Next/Previous code buffer
(defun next-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name)))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name))))
      (next-buffer))))
(defun previous-code-buffer ()
  (interactive)
  (let (( bread-crumb (buffer-name)))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not ( equal bread-crumb (buffer-name))))
      (previous-buffer))))
(global-set-key (kbd "M-") 'next-code-buffer)
(global-set-key (kbd "M-") 'previous-code-buffer)


(provide 'init-ido)
;;; init-ido.el ends here
