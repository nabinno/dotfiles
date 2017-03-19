;;; init-git --- git configuration
;;; Commentary:
;;; Code:
(require-package 'magit)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger) ;; Though see also vc-annotate's "n" & "p" bindings
(require-package 'git-timemachine)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

;; coloration
;; (custom-set-faces
;;  '(diff-added ((t (:foreground "#149914" :background nil :inherit nil))))
;;  '(diff-removed ((t (:foreground "#991414" :background nil :inherit nil)))))
(custom-set-faces
 '(magit-diff-added ((t (:background "black" :foreground "green"))))
 '(magit-diff-added-highlight ((t (:background "black" :foreground "green"))))
 '(magit-diff-removed ((t (:background "black" :foreground "pink"))))
 '(magit-diff-removed-highlight ((t (:background "black" :foreground "pink"))))
 '(magit-hash ((t (:foreground "red")))))

;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.

(when (maybe-require-package 'magit)
  (setq-default
   magit-process-popup-time 10
   magit-diff-refine-hunk t
   magit-completing-read-function 'magit-ido-completing-read)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

;; (require-package 'fullframe)
;; (after-load 'magit
;;   (fullframe magit-status magit-mode-quit-window))


;; ;;; When we start working on git-backed files, use git-wip if available
;;
;; (after-load 'magit
;;   (global-magit-wip-save-mode)
;;   (diminish 'magit-wip-save-mode))
;;
;; (after-load 'magit
;;   (diminish 'magit-auto-revert-mode))


(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))


;; ;;; Git blame
;; (require-package 'git-blame)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)


;;; Git flow
(require-package 'magit-gitflow)
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)


;;; Git gutter
(require-package 'git-gutter+)
(global-git-gutter+-mode)

(eval-after-load 'git-gutter+
 '(progn
    (define-key git-gutter+-mode-map (kbd "C-x n h")     'git-gutter+-next-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x n p")   'git-gutter+-previous-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x n v =") 'git-gutter+-show-hunk)
    (define-key git-gutter+-mode-map (kbd "C-x n r")   'git-gutter+-revert-hunks)
    (define-key git-gutter+-mode-map (kbd "C-x n t")   'git-gutter+-stage-hunks)
    (define-key git-gutter+-mode-map (kbd "C-x n c")   'git-gutter+-commit)
    (define-key git-gutter+-mode-map (kbd "C-x n C")   'git-gutter+-stage-and-commit)
    (define-key git-gutter+-mode-map (kbd "C-x n C-y") 'git-gutter+-stage-and-commit-whole-buffer)
    (define-key git-gutter+-mode-map (kbd "C-x n U")   'git-gutter+-unstage-whole-buffer)
    (setq git-gutter+-modified-sign "  ")
    (setq git-gutter+-added-sign "++")
    (setq git-gutter+-deleted-sign "--")
    (set-face-background 'git-gutter+-modified "purple")
    (set-face-foreground 'git-gutter+-added "green")
    (set-face-foreground 'git-gutter+-deleted "red")
    ))


;;; Git grep
(require-package 'helm-git-grep)
(global-set-key (kbd "C-c ; G") 'helm-git-grep)
(define-key isearch-mode-map (kbd "C-c ; G") 'helm-git-grep-from-isearch)
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c ; G") 'helm-git-grep-from-helm))


;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)


;; ;;; git-svn support
;;
;; (require-package 'magit-svn)
;; (autoload 'magit-svn-enabled "magit-svn")
;; (defun sanityinc/maybe-enable-magit-svn-mode ()
;;   (when (magit-svn-enabled)
;;     (magit-svn-mode)))
;; (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode)
;;
;; (after-load 'compile
;;   (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
;;                       '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
;;     (add-to-list 'compilation-error-regexp-alist-alist defn)
;;     (add-to-list 'compilation-error-regexp-alist (car defn))))
;;
;; (defvar git-svn--available-commands nil "Cached list of git svn subcommands")
;;
;; (defun git-svn (dir)
;;   "Run a git svn subcommand in DIR."
;;   (interactive "DSelect directory: ")
;;   (unless git-svn--available-commands
;;     (setq git-svn--available-commands
;;           (sanityinc/string-all-matches
;;            "^  \\([a-z\\-]+\\) +"
;;            (shell-command-to-string "git svn help") 1)))
;;   (let* ((default-directory (vc-git-root dir))
;;          (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
;;     (compile (concat "git svn "
;;                      (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))


(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)


(provide 'init-git)
;;; init-git.el ends here
