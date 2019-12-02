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
    (ruby-hash-syntax move-dup expand-region highlight-symbol unfill google-translate undo-tree whole-line-or-region visual-regexp scratch page-break-lines multiple-cursors markdown-mode exec-path-from-shell elscreen el-get diminish)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 102 :width normal :foundry "outline" :family "MS Gothic"))))
 '(cursor ((t (:background "green"))))
 '(fixed-pitch ((t (:family "MS Gothic")))))
(add-to-list 'default-frame-alist '(font . "MS Gothic"))
(set-face-attribute 'default nil :font "MS Gothic" :height 120)
(set-frame-font "MS Gothic" nil t)
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


;;; MELPA - Standard package repositories
(when (< emacs-major-version 24)
  ;; Mainly for ruby-mode
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))
;; We include the org repository for completeness, but don't normally use it.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;;; Also use Melpa for most packages
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


;;; el-get
(require-package 'el-get)
(add-to-list 'load-path (expand-file-name "el-get" user-emacs-directory))


;;; exec-path-from-shell
(require-package 'exec-path-from-shell)
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
(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-undo)


;;; page-break-lines-char
(require-package 'diminish)
(require-package 'scratch)
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)
(add-hook 'prog-mode-hook 'page-break-lines-mode)
(setq page-break-lines-char ?-)


;;; unfill and whole-line-or-region
(require-package 'unfill)
(require-package 'whole-line-or-region)
(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))


;;; Show matching parens
(show-paren-mode 1)


;;; multiple-cursors
(require-package 'multiple-cursors)
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
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))


;;; whole-line-or-region-mode
(require-package 'whole-line-or-region)
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
(require-package 'visual-regexp)
(define-key global-map (kbd "M-r") 'vr/replace)
(define-key global-map (kbd "C-M-m") 'vr/mc-mark)


;;; elscreen
(require-package 'elscreen)
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
(global-set-key (kbd "<f5>") 'elscreen-create)
(elscreen-start)


;;; highlight-symbol
(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))


;;; Expand region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-=") 'er/expand-region)


;;; move-dup
(require-package 'move-dup)
(global-set-key [M-up]              'md-move-lines-up)
(global-set-key [M-down]            'md-move-lines-down)
(global-set-key [M-S-up]            'md-move-lines-up)
(global-set-key [M-S-down]          'md-move-lines-down)
(global-set-key (kbd "C-c p") 'md-duplicate-down)


;;; Ruby
(require-package 'ruby-mode)
(require-package 'ruby-hash-syntax)
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
(unless (require 'rufo nil 'noerror)
  (el-get-bundle danielma/rufo.el))
(add-hook 'ruby-mode-hook 'rufo-minor-mode)


;;; markdown-mode
(require-package 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\|apib\\)\\'" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (define-key markdown-mode-map (kbd "M-e") 'markdown-cycle)
	    ;; (hide-sublevels 2)
	    ))


;;; org-mode
(when (< emacs-major-version 24)
  (require-package 'org))
;; (require-package 'org-fstree) ;;; TODO

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
(require-package 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)



(require 'server)
(unless (server-running-p)
  (server-start))



(provide '.emacs)
;;; .emacs ends here
