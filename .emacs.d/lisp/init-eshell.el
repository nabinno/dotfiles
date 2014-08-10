; shell settings
; ==============
; eshell
; ------
;; (require 'eshell-pop)
(setq eshell-directory-name "~/.emacs.d/eshell/")
(setq shell-pop-window-height 30)
;;(require 'esh-myparser)
(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
(eval-after-load 'esh-opt
  '(progn
     (require 'em-cmpl)
     (require 'em-prompt)
     (require 'em-term)
     ;; TODO: for some reason requiring this here breaks it, but
     ;; requiring it after an eshell session is started works fine.
     ;; (require 'eshell-vc)
     (setenv "PAGER" "cat")
    ;; (set-face-attribute 'eshell-prompt nil :foreground "turquoise1")
     (add-hook 'eshell-mode-hook ;; for some reason this needs to be a hook
               '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
     ;; (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)
     ))
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
		    ))))


; tramp
; -----
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-debug-buffer t)
(setq tramp-completion-without-shell-p t)


; zsh
; ---
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



(provide 'init-eshell)
