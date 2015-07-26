(require-package 'cperl-mode)
;; (load-library "cperl-mode")

(defalias 'perl-mode 'cperl-mode)
(setenv "PKG_DBDIR" (concat (getenv "HOME") "/local/var/db/pkg"))
(setenv "PORT_DBDIR" (concat (getenv "HOME") "/local/var/db/pkg"))
(setenv "INSTALL_AS_USER")
(setenv "LD_LIBRARY_PATH" (concat (getenv "HOME") "/local/lib"))
(setenv "TMPDIR" (concat (getenv "HOME") "/local/tmp"))
(setenv "MODULEBUILDRC" (concat (getenv "HOME") "/local/.modulebuildrc"))
(setenv "PATH" (concat (getenv "HOME") "/local/bin:" (getenv "PATH")))
(setenv "PERL_MM_OPT" "INSTALL_BASE=~/local")
(setenv "PERL5LIB" (concat "~/local/lib/perl5:~/local/lib/perl5/i686-cygwin-thread-multi-64int:" (getenv "PERL5LIB")))
(setenv "PERL_CPANM_OPT" (concat "-l " (getenv "HOME") "/local --mirror " (getenv "HOME") "/.cpan/minicpan/"))
(setq cperl-indent-level 4
      cperl-continued-statement-offset 4
      cperl-brace-offset -4
      cperl-label-offset -4
      cperl-indent-parens-as-block t
      cperl-close-paren-offset -4
      ;; cperl-electric-parens t
      ;; cperl-invalid-face nil
      cperl-tab-always-indent t
      cperl-highlight-variables-indiscriminately t
      )

(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.[Pp][LlMm]Cc]?$" . cperl-mode))
(while (let ((orig (rassoc 'perl-mode auto-mode-alist)))
	 (if orig (setcdr orig 'cperl-mode))))
(dolist (interpreter '("perl" "perl5" "miniperl" "pugs"))
  (unless (assoc interpreter interpreter-mode-alist)
    (add-to-list 'interpreter-mode-alist (cons interpreter 'cperl-mode))))


;;; Use % to match various kinds of brackets

;; (defun match-paren (arg)
;;  "Go to the matching paren if on a paren; otherwide insert %."
;;  (interactive "p")
;;  (let ((prev-char (char-to-string (preceding-char)))
;; 	(next-char (char-to-string (following-char))))
;;    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
;; 	  ((string-match "[\]})>" prev-char) (backward-sexp 1))
;; 	  (t (self-insert-command (or arg 1))))))
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "%" 'match-paren)


;;; Perl Tidy
(require-package 'perltidy)
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
  (perltidy-region)))


;;; Key Bind and more
(require-package 'perl-completion)
(add-hook 'cperl-mode-hook
	  (lambda ()
	    (require-package 'auto-complete)
	    ;; (require-package 'perl-completion)
	    (perl-completion-mode t)
	    (setq auto-fill-mode t
                  cperl-array-face 'cperl-array-face
                  cperl-hash-face 'cperl-hash-face
                  fill-column 78
                  indent-tabs-mode nil)
	    (set-face-background 'cperl-array-face (face-background 'default))
	    (set-face-background 'cperl-hash-face (face-background 'default))
	    ;; (set-face-foreground 'cperl-array-face "color-69")
	    ;; (make-face 'cperl-array-face)
	    (add-to-list 'ac-sources 'ac-source-perl-completion)
	    (mapc (lambda (pair)
		    (let ((key (car pair))
			  (func (cdr pair)))
		      (define-key cperl-mode-map
			(read-kbd-macro key) func)))
		  '(("C-c t" . perltidy-region)
		    ("C-c C-t" . perltidy-defun)
		    ("C-c m" . plcmp-cmd-menu)
		    ("C-c s" . plcmp-cmd-smart-complete)
		    ("C-c d" . plcmp-cmd-show-doc)
		    ("C-c p" . plcmp-cmd-show-doc-at-point)
		    ("C-c c" . plcmp-cmd-clear-all-cashes)
		    ("TAB" . cperl-indent-region)
		    ;; ("M-C-i" . perltidy)
                    ("M-," . cperl-perldoc)
                    ("M-C-," . plcmp-cmd-show-doc-at-point)
                    ("M-[ 1 ; 7 l" . plcmp-cmd-show-doc-at-point)
		    ))))


;; Flymake
(require-package 'flymake)
(require-package 'set-perl5lib)

;; Flymake for JavaScript
;; (defconst flymake-allowed-js-file-name-masks
;;   '(("\\.user\\.js$" flymake-js17-init)
;;     ("\\.json$" flymake-js-init)
;;     ("\\.js$" flymake-js-init)))
;; (defcustom flymake-js-detect-trailing-comma t nil :type 'boolean)
;; (defvar flymake-js-err-line-patterns '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(SyntaxError\:.+\\)\:$" 1 2 nil 3)))
;; (when flymake-js-detect-trailing-comma
;;   (setq flymake-js-err-line-patterns (append flymake-js-err-line-patterns
;;                                              '(("^\\(.+\\)\:\\([0-9]+\\)\: \\(strict warning: trailing comma.+\\)\:$" 1 2 nil 3)))))
;; (defun flymake-js-init ()    ; .js書くときはJavaScript 1.6相当。user.js書くときは1.7相当でsyntaxチェック
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "~/app/spidermonkey-1.6/js" (list "-sC" local-file))))
;; (defun flymake-js17-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      'flymake-create-temp-inplace))
;;          (local-file (file-relative-name
;;                       temp-file
;;                       (file-name-directory buffer-file-name))))
;;     (list "js" (list "-sC" local-file))))
;; (defun flymake-js-load ()
;;   (interactive)
;;   (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;     (setq flymake-check-was-interrupted t))
;;   (ad-activate 'flymake-post-syntax-check)
;;   (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-js-file-name-masks))
;;   (setq flymake-err-line-patterns flymake-js-err-line-patterns)
;;   (flymake-mode t))
;; (add-hook 'javascript-mode-hook '(lambda () (flymake-js-load)))

;; Flymake for Perl
(defvar flymake-perl-err-line-patterns
  '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))
(defconst flymake-allowed-perl-file-name-masks
  '(("\\.pl$" flymake-perl-init)
    ("\\.pm$" flymake-perl-init)
    ("\\.t$" flymake-perl-init)))
(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc" local-file))))
(defun flymake-perl-load ()
  (interactive)
  (set-perl5lib)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (flymake-mode t))
(add-hook 'cperl-mode-hook '(lambda () (flymake-perl-load)))
(defun next-flymake-error ()
  (interactive)
  (flymake-goto-next-error)
  (let ((err (get-char-property (point) 'help-echo)))
    (when err
      (message err))))
(global-set-key (kbd "M-E") 'next-flymake-error)



(provide 'init-perl)
