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
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
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


;;; page-break-lines-char
(require-package 'diminish)
(require-package 'scratch)
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)
(add-hook 'prog-mode-hook 'page-break-lines-mode)
(setq page-break-lines-char ?-)


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


;;; markdown-mode
(require-package 'markdown-mode)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\|apib\\)\\'" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (define-key markdown-mode-map (kbd "M-e") 'markdown-cycle)
	    ;; (hide-sublevels 2)
	    ))


(provide '.emacs)
;;; .emacs ends here
