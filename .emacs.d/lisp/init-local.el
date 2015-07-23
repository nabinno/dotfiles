; Basic setting
; ==============
(setq default-directory "~/")


; after init hook
; ---------------
(defvar *on_linux* (string-match "linux" system-configuration)) (when *on_linux* (add-hook 'after-init-hook 'server-start) (add-hook 'server-done-hook (lambda () (shell-command "screen -X select $(cat ~/.emacs.d/emacsclient-caller)"))))


; other
; -----
(cond (window-system (setq x-select-enable-clipboard t)))
(defun my-bell-function () (unless (memq this-command '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit mwheel-scroll down up next-line previous-line backward-char forward-char)) (ding))) (setq ring-bell-function 'my-bell-function)
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t)
(iswitchb-mode t)
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
;; (tool-bar-mode 0)
(transient-mark-mode t)

; ### display-time-mode ###
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-string-forms
      '((let
	    ((system-time-locale "C"))
	  (format-time-string " [%R %d %b %a] " now))))


; paren
; -----
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)


;; ; deffered
;; ; --------
;; (require 'cl)
;; (require 'deferred)


; info
; ----
(require 'info)
;; (add-to-list 'Info-additional-directory-list "/usr/share/info")
(add-to-list 'Info-directory-list "~/info")



; key bind settings
; =================
(defun my-kill-some-buffers () (interactive) (dolist (buffer (buffer-list)) (unless (string= (buffer-name buffer) "*scratch*")) (kill-buffer buffer))) (global-set-key (kbd "C-x C-k") 'my-kill-some-buffers)
(global-set-key (kbd "C-x w") 'toggle-truncate-lines)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-r") 'replace-string)
(global-set-key (kbd "C-M-r") 'replace-regexp)
(global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "C-M-n") 'next-buffer)
;; (global-set-key (kbd "C-M-p") 'previous-buffer)
;; (global-set-key (kbd "M-n") 'forward-paragraph)
;; (global-set-key (kbd "M-P") 'backward-paragraph)
;; (global-set-key (kbd "C-M-l") 'recenter)
(global-set-key (kbd "C-S-i") 'indent-region)
;; (global-set-key (kbd "M-TAB") 'indent-region)
;; (global-set-key [f1] 'help-for-help)
;; (global-set-key (kbd "C-x e") 'electric-buffer-list)
(global-set-key (kbd "M-SPC") 'ibuffer)
(global-set-key (kbd "C-x C-z") 'shell-pop)
(global-set-key (kbd "M-9") '(lambda () (interactive) (find-file "~/.zshrc") (sh-mode)))
(global-set-key (kbd "M-0") '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c e") '(lambda () (interactive) (eshell)))
(global-set-key (kbd "M-1") '(lambda () (interactive) (eshell)))
(global-set-key (kbd "C-c s") '(lambda () (interactive) (shell)))
(global-set-key (kbd "M-C-1") '(lambda () (interactive) (shell)))
(global-set-key (kbd "M-[ 1 ; 7 q") '(lambda () (interactive) (shell)))
;; (keyboard-translate ?\C-h ?\C-?) (global-set-key (kbd "C-h") nil)
;; (global-set-key (kbd "C-x C-h") 'help-command)
(global-set-key (kbd "M-s") 'view-mode)
(global-set-key (kbd "<f9>") '(lambda () (interactive) (hl-line-mode) (view-mode)))
(global-set-key (kbd "M-[ 1 ; 5 i") 'other-window)
(global-set-key (kbd "C-TAB") 'other-window)
(global-unset-key (kbd "C-z"))


; view
; ----
(setq view-read-only t)
(defvar pager-keybind
  '(("h" . backward-char)
    ("l" . forward-char)
    ("J" . next-line)
    ("K" . previous-line)
    ("x" . kill-this-buffer)
    (";" . kill-this-buffer)
    ("b" . scroll-down)
    (" " . scroll-up)
    ("g" . beginning-of-buffer)
    ("p" . beginning-of-buffer)
    ("e" . end-of-buffer)
    ("n" . end-of-buffer)
    ("j" . forward-list)
    ("k" . backward-list)
    ;; ("j" . (lambda () (interactive) (scroll-up 4)))
    ;; ("k" . (lambda () (interactive) (scroll-down 4)))
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



; any mode settings
; =================
; yaml
; ----
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;; ; autocomplete
;; ; ------------
;; (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
;; (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
;; (defadvice ac-on-post-command
;;   (around check-whether-input-type-is-japanese activate)
;;   (or current-input-method ad-do-it))



; buffer
; ======
; org mode
; --------
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c B") 'googlecl-prompt-blog)
(global-set-key (kbd "C-c L") 'org-googlecl-list-blogs)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-hide-leading-stars t)
(setq org-directory "~/")
(setq org-default-notes-file "notes.org")
(setq org-tag-alist
      '(
	("Heroku" . ?k)
	("Rails" . ?r)
	))

; ### html ###
(setq org-export-html-validation-link nil)
(setq org--html-postamble nil)
(setq org-html-validation-link nil)

; ### agenda ###
(setq org-agenda-files (list org-directory))
(setq calendar-holidays nil)

; ### key bind ###
(add-hook 'org-mode-hook
	  (lambda ()
	    (mapc (lambda (pair)
		    (let ((key (car pair))
			  (func (cdr pair)))
		      (define-key org-mode-map
			(read-kbd-macro key) func)))
		  '(("C-<tab>" . other-window)
		    ("M-[ 1 ; 5 i" . other-window)
		    ("S-M-j" . org-insert-todo-heading)
		    ("M-J" . org-insert-todo-heading)
		    ("?" . org-insert-subheading)
		    ("?" . org-backward-heading-same-level)
		    ("?" . org-forward-heading-same-level)
		    ("C-M-p" . org-move-subtree-up)
		    ("C-M-n" . org-move-subtree-down)
		    ("<prior>" . org-shiftup)
		    ("M-o h" . org-shiftleft)
		    ("M-o f" . org-shiftright)
		    ("<next>" . org-shiftdown)
		    ("ESC <prior>" . org-shiftmetaup)
		    ("ESC M-[ 5 ~" . org-shiftmetaup)
		    ("ESC M-O h" . org-shiftmetaleft)
		    ("ESC M-O f" . org-shiftmetaright)
		    ("ESC <next>" . org-shiftmetadown)
		    ("ESC M-[ 6 ~" . org-shiftmetadown)
		    ;; ("" . org-shiftcontrolup)
		    ;; ("" . org-shiftcontrolleft)
		    ;; ("" . org-shiftcontrolright)
		    ;; ("" . org-shiftcontroldown)
		    ))))


;; ; zlc
;; ; ---
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


;; ; lcomp
;; ; -----
;; (require 'lcomp)
;; (lcomp-install)


; diff
; ----
(setq ediff-window-setup-function 'ediff-setup-windows-plain)



; Other
; =====
(put 'scroll-left 'disabled nil)




(provide 'init-local)
