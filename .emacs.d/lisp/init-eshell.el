(setq eshell-directory-name "~/.emacs.d/eshell/")
(setq eshell-cmpl-cycle-completions nil)
(setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
(setq eshell-save-history-on-exit t)

(eval-after-load 'esh-opt
  '(progn
     ;; (require 'em-cmpl)
     (require 'em-prompt)
     (require 'em-term)
     ;; TODO: for some reason requiring this here breaks it, but
     ;; requiring it after an eshell session is started works fine.
     ;; (require 'eshell-vc)
     (setenv "PAGER" "cat")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
               '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     ))

(defun eshell--initialize ()
  (interactive)
  (progn
    (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)
    (add-to-list 'eshell-output-filter-functions
                 '(lambda ()
                    (save-excursion
                      (replace-regexp "[-\\|/]?\[[0-9m]+[GKJ]?" "" nil
                                      eshell-last-output-start eshell-last-output-end))))
    (add-to-list 'eshell-command-completions-alist
                 '("gunzip" "gz\\'"))
    (add-to-list 'eshell-command-completions-alist
                 '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
    (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
    ))

(defun eshell--delete-other-windows-and-speedbar-close ()
  (interactive)
  (progn
    (sanityinc/toggle-delete-other-windows)
    (sr-speedbar-close)))
(defun eshell/cdp ()
  (let* ((cmd "git rev-parse --show-toplevel")
         (dir (with-temp-buffer
                (unless (call-process-shell-command cmd nil t)
                  (error "Here is not Git Repository"))
                (goto-char (point-min))
                (buffer-substring-no-properties
                 (point) (line-end-position)))))
        (eshell/cd dir)))

;; keybind
(add-hook 'eshell-mode-hook
	  (lambda ()
	    ;; (set-buffer-process-coding-system 'utf-8 'utf-8)
	    (mapc (lambda (pair)
		    (let ((key (car pair))
			  (func (cdr pair)))
		      (define-key eshell-mode-map
			(read-kbd-macro key) func)))
		  '(("M-s" . (lambda () (interactive) (hl-line-mode) (view-mode)))
		    ("C-M-l" . recenter)
		    ("<backtab>" . eshell--delete-other-windows-and-speedbar-close)
		    ))))


;;; Multi eshell
(require-package 'multi-eshell)
(setq multi-eshell-shell-function '(eshell))
(setq multi-eshell-name "*eshell*")

(defun multi-eshell--kill-all ()
  (interactive)
  (dolist
      (buffer (buffer-list))
    (if (string-match-p"^\*eshell"  (buffer-name buffer))
        (kill-buffer buffer))))

(global-set-key (kbd "M-1") 'multi-eshell-switch)
(global-set-key (kbd "M-!") 'multi-eshell)


;; ;;; Eshell Z
;; (eval-after-load 'eshell
;;       '(require-package 'eshell-z nil t))


;; ;;; Eshell myparser
;; (unless (require 'esh-myparser nil 'noerror)
;;   (el-get-bundle! esh-myparser
;;     :url "http://www.emacswiki.org/emacs/download/esh-myparser.el"))


;; ;;; Eshell pop
;; (require 'eshell-pop)
;; (setq shell-pop-window-height 30)


;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-debug-buffer t)
(setq tramp-completion-without-shell-p t)


;; Zsh
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
(setq system-uses-terminfo nil)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key (kbd "C-c s") '(lambda () (interactive) (shell)))
(global-set-key (kbd "M-C-1") '(lambda () (interactive) (shell)))
(global-set-key (kbd "M-[ 1 ; 7 q") '(lambda () (interactive) (shell)))


(provide 'init-eshell)
