;;; dotemacs --- Package configuration
;;; Commentary:
;;; Code:
(package-initialize)


;;; Local configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (csv-mode ruby-hash-syntax move-dup expand-region highlight-symbol unfill google-translate undo-tree whole-line-or-region visual-regexp scratch page-break-lines multiple-cursors markdown-mode exec-path-from-shell elscreen el-get diminish)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(truncate-lines t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "green" :type "box"))))
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 102 :width normal :foundry "outline" :family "Source Han Mono"))))
 '(fixed-pitch ((t (:family "Source Han Mono")))))

(add-to-list 'default-frame-alist '(font . "Source Han Mono"))
(add-to-list 'default-frame-alist '(cursor-type . box))
(add-to-list 'default-frame-alist '(cursor-color . "green"))
(set-face-attribute 'default nil :font "Source Han Mono" :height 110)
(set-frame-font "Source Han Mono" nil t)

(global-set-key (kbd "M-0") '(lambda () (interactive) (progn (find-file "~/.emacs") (delete-other-windows))))
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-t") 'quoted-insert)


;;; background
(set-frame-parameter (selected-frame) 'alpha '(80 80))


;;; display-time-mode
(display-time-mode t)
(setq display-time-24hr-format t)
(setq display-time-string-forms
      '((let
            ((system-time-locale "C"))
          (format-time-string " [%R %d %b %a] " now))))


;;; auto-save-mode
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;; Rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))


;;; straight.el
(defvar bootstrap-version)
  (let ((bootstrap-file
             (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
            (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
          (with-current-buffer
                  (url-retrieve-synchronously
                   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                   'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
(use-package use-package-ensure-system-package :straight t)

;; base
(defun jjin/use-package-if-prehook (name _keyword pred rest state)...)
(advice-add 'use-package-handler/:if :before 'jjin/use-package-if-prehook)

(use-package diminish :straight t)
(use-package names :straight t)
(use-package system-packages
  :straight t
  :init
  (setq system-packages-use-sudo nil)
  (when (eq system-type 'darwin)
    (setq system-packages-package-manager 'brew)))


;;; exec-path-from-shell
(use-package exec-path-from-shell :straight t)
(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))
(after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(setenv "PATH" (concat "C:\\Program Files\\Git\\cmd;" (getenv "PATH")))


;;; undo-tree
(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode)
  (global-set-key (kbd "M-/") 'undo-tree-undo))


;;; page-break-lines-char
(use-package diminish :straight t)
(use-package scratch :straight t)
(use-package page-break-lines
  :straight t
  :config
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (setq page-break-lines-char ?-))


;;; unfill and whole-line-or-region
(use-package unfill :straight t)
(use-package whole-line-or-region :straight t)
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))


;;; Show matching parens
(show-paren-mode 1)


;;; multiple-cursors
(use-package multiple-cursors
  :straight t
  :config
  ;; multiple-cursors
  (global-set-key (kbd "M-,")             'mc/mark-previous-like-this)
  (global-set-key (kbd "C-<")             'mc/mark-previous-like-this)
  ;; (global-set-key (kbd "M-[ 1 ; 6 l")     'mc/mark-previous-like-this)
  (global-set-key (kbd "M-.")             'mc/mark-next-like-this)
  (global-set-key (kbd "C->")             'mc/mark-next-like-this)
  ;; (global-set-key (kbd "M-[ 1 ; 6 n")     'mc/mark-next-like-this)
  (global-set-key (kbd "C-+")             'mc/mark-next-like-this)
  (global-set-key (kbd "M-+")             'mc/mark-next-like-this)
  ;; (global-set-key (kbd "M-[ 1 ; 6 k")     'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<")         'mc/mark-all-like-this)
  ;; (global-set-key (kbd "C-c M-[ 1 ; 6 l") 'mc/mark-all-like-this)
  ;; From active region to multiple cursors:
  (global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-c c c") 'mc/edit-lines)
  (global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)
  ;; To be able to M-x without meta
  (global-set-key (kbd "C-x C-m") 'execute-extended-command)
  ;; Vimmy alternatives to M-^ and C-u M-^
  (global-set-key (kbd "C-c j") 'join-line)
  (global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1))))


;;; whole-line-or-region-mode
(use-package whole-line-or-region :straight t)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)
(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))
(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)


;;; visual-regex
(use-package visual-regexp
  :straight t
  :config
  (define-key global-map (kbd "M-r") 'vr/replace)
  (define-key global-map (kbd "C-M-m") 'vr/mc-mark))


;;; elscreen
(use-package elscreen
  :straight t
  :config
  (setq elscreen-display-tab nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-buffer-to-nickname-alist
	'(("^dired-mode$"     . (lambda () (format "Dired(%s)" dired-directory)))
          ("^Info-mode$"      . (lambda () (format "Info(%s)" (file-name-nondirectory Info-current-file))))
          ("^mew-draft-mode$" . (lambda () (format "Mew(%s)" (buffer-name (current-buffer)))))
          ("^mew-"            . "Mew")
          ("^irchat-"         . "IRChat")
          ("^liece-"          . "Liece")
          ("^lookup-"         . "Lookup")))
  (setq elscreen-mode-to-nickname-alist
	'(("[Ss]hell"     . "shell")
          ("compilation"  . "compile")
          ("-telnet"      . "telnet")
          ("dict"         . "OnlineDict")
          ("*WL:Message*" . "Wanderlust")))
  (global-set-key (kbd "M-)") '(lambda () (interactive) (elscreen-goto 0)))
  (global-set-key (kbd "M-!") '(lambda () (interactive) (elscreen-goto 1)))
  (global-set-key (kbd "M-@") '(lambda () (interactive) (elscreen-goto 2)))
  (global-set-key (kbd "M-#") '(lambda () (interactive) (elscreen-goto 3)))
  (global-set-key (kbd "M-$") '(lambda () (interactive) (elscreen-goto 4)))
  (global-set-key (kbd "M-%") '(lambda () (interactive) (elscreen-goto 5)))
  (global-set-key (kbd "M-^") '(lambda () (interactive) (elscreen-goto 6)))
  (global-set-key (kbd "M-&") '(lambda () (interactive) (elscreen-goto 7)))
  (global-set-key (kbd "M-*") '(lambda () (interactive) (elscreen-goto 8)))
  (global-set-key (kbd "M-(") '(lambda () (interactive) (elscreen-goto 9)))
  (global-set-key (kbd "<f5>") 'elscreen-create))
(elscreen-start)


;;; highlight-symbol
(use-package highlight-symbol
  :straight t
  :config
  (dolist (hook '(prog-mode-hook html-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
  (eval-after-load 'highlight-symbol
    '(diminish 'highlight-symbol-mode)))


;;; Expand region
(use-package expand-region
  :straight t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "M-=") 'er/expand-region))


;;; move-dup
(use-package move-dup
  :straight t
  :config
  (global-set-key [M-up]        'move-dup-move-lines-up)
  (global-set-key [M-down]      'move-dup-move-lines-down)
  (global-set-key [M-S-up]      'move-dup-move-lines-up)
  (global-set-key [M-S-down]    'move-dup-move-lines-down)
  (global-set-key (kbd "C-c p") 'move-dup-duplicate-down))


;;; Ruby
(use-package ruby-mode :straight t)
(use-package ruby-hash-syntax :straight t)
(setq ruby-use-encoding-map nil)
(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))
(add-hook 'ruby-mode-hook 'subword-mode)
(use-package rufo
  :straight (:host github :repo "danielma/rufo.el")
  :config
  (add-hook 'ruby-mode-hook 'rufo-minor-mode))


;;; markdown-mode
(use-package markdown-mode
  :straight t
  :config
  (setq auto-mode-alist
	(cons '("\\.\\(md\\|markdown\\|apib\\)\\'" . markdown-mode) auto-mode-alist))
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (define-key markdown-mode-map (kbd "M-e") 'markdown-cycle)
	      ;; (hide-sublevels 2)
	      ))
  (set-face-attribute 'markdown-code-face nil :inherit 'default :foreground "light green"))


;;; org-mode
(use-package org :straight t)
(use-package org-fstree :straight t)

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))


;;; translation
(use-package google-translate
  :straight t
  :config
  (require 'google-translate-default-ui)
  (global-set-key "\C-ct" 'google-translate-at-point)
  (global-set-key "\C-cT" 'google-translate-query-translate))


(require 'server)
(unless (server-running-p)
  (server-start))



(provide '.emacs)
;;; .emacs ends here
