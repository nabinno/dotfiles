;;; Basic setting
(setq default-directory "~/")

;; after init hook
(defvar *on_linux*
  (string-match "linux" system-configuration))
(when *on_linux*
  (add-hook 'after-init-hook
            'server-start)
  (add-hook 'server-done-hook
            (lambda () (shell-command "screen -X select $(cat ~/.emacs.d/emacsclient-caller)"))))

;; other
(cond (window-system (setq x-select-enable-clipboard t)))
(defun my-bell-function ()
  (unless
      (memq this-command
            '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit
                            mwheel-scroll down up next-line previous-line
                            backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t)
;; (iswitchb-mode t)
(menu-bar-mode 0)
(put 'narrow-to-defun 'disabled t)
(put 'narrow-to-page 'disabled t)
(put 'narrow-to-region 'disabled t)
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
;; (scroll-bar-mode 0)
(setq cache-long-line-scans t)
(setq delete-by-moving-to-trash t)
(setq echo-keystrokes 0.0001)
(setq frame-title-format "%B (%F)")
(setq gc-cons-threshold 10000000)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq message-log-max 512)
(set-default 'truncate-lines t)
;; (tool-bar-mode 0)
(transient-mark-mode t)

;; display-time-mode
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-string-forms
      '((let
	    ((system-time-locale "C"))
	  (format-time-string " [%R %d %b %a] " now))))

;; auto-save-mode
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; paren
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)

;; ;; deffered
;; (require 'cl)
;; (require 'deferred)

;; info
(require 'info)
;; (add-to-list 'Info-additional-directory-list "/usr/share/info")
(add-to-list 'Info-directory-list "~/info")


;;; Key Bind Settings
(defun my-kill-some-buffers ()
  (interactive)
  (dolist
      (buffer (buffer-list))
    (unless
        (string= (buffer-name buffer) "*scratch*"))
    (kill-buffer buffer)))
(global-set-key (kbd "C-x C-k") 'my-kill-some-buffers)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x w") 'toggle-truncate-lines)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-r") 'replace-string)
(global-set-key (kbd "C-M-r") 'replace-regexp)
(global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-n") 'forward-paragraph)
;; (global-set-key (kbd "M-P") 'backward-paragraph)
;; (global-set-key (kbd "C-M-l") 'recenter)
(global-set-key (kbd "C-S-i") 'indent-region)
;; (global-set-key (kbd "M-TAB") 'indent-region)
;; (global-set-key [f1] 'help-for-help)
(global-set-key (kbd "M-9") '(lambda () (interactive) (progn (find-file "~/.zshrc") (sh-mode))))
(global-set-key (kbd "M-0") '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
;; (keyboard-translate ?\C-h ?\C-?) (global-set-key (kbd "C-h") nil)
;; (global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "C-t") 'quoted-insert)
(global-set-key (kbd "") 'indent-for-tab-command)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "<f2>"))


;;; Any mode settings
;; yaml
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;; Buffer
;; ;; zlc
;; (require 'zlc)
;; (setq zlc-select-completion-immediately t)
;; (let ((map minibuffer-local-map))
;;   (define-key map (kbd "<backtab>") 'zlc-select-previous)
;;   (define-key map (kbd "S-<tab>") 'zlc-select-previous)
;;   (define-key map (kbd "C-M-p") 'zlc-select-previous-vertical)
;;   (define-key map (kbd "C-M-n") 'zlc-select-next-vertical)
;;   (define-key map (kbd "C-p") 'zlc-select-previous)
;;   (define-key map (kbd "C-n") 'zlc-select-next)
;;   )

;; ;; lcomp
;; (require 'lcomp)
;; (lcomp-install)

;; diff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)



;;; Other
(put 'scroll-left 'disabled nil)




(provide 'init-local)
