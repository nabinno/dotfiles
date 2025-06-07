;;; init-git --- git configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf gitignore-mode :ensure t)
(leaf gitconfig-mode :ensure t)
(leaf git-messenger :ensure t) ;; Though see also vc-annotate's "n" & "p" bindings
(leaf git-timemachine :ensure t)

(leaf magit
  :ensure t
  :config
  (setq-default
   magit-save-some-buffers nil
   magit-process-popup-time 10
   magit-diff-refine-hunk t
   magit-completing-read-function 'magit-ido-completing-read)
  ;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  ;; coloration
  (custom-set-faces
   '(magit-section-highlight ((t (:background "transparent"))))
   '(magit-diff-added ((t (:background "black" :foreground "green"))))
   '(magit-diff-added-highlight ((t (:background "black" :foreground "green"))))
   '(magit-diff-removed ((t (:background "black" :foreground "pink"))))
   '(magit-diff-removed-highlight ((t (:background "black" :foreground "pink"))))
   '(magit-diff-context-highlight ((t (:background "transparent"))))
   '(magit-hash ((t (:foreground "red")))))
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section)
  )

;; (leaf fullframe
;;   :ensure t
;;   :after magit
;;   :config
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
;; (leaf git-blame :ensure t)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)


;;; Git flow
(leaf magit-gitflow
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))


;; ;;; Git gutter
;; (leaf git-gutter-fringe
;;   :ensure t
;;   :config
;;   (require 'git-gutter-fringe)
;;   (global-git-gutter-mode +1)
;;   (setq git-gutter-fr:side 'left-fringe)
;;   (setq git-gutter:modified-sign "  ")
;;   (setq git-gutter:added-sign "++")
;;   (setq git-gutter:deleted-sign "--")
;;   (set-face-foreground 'git-gutter-fr:modified "purple")
;;   (set-face-foreground 'git-gutter-fr:added "green")
;;   (set-face-foreground 'git-gutter-fr:deleted "red")
;;   (global-set-key (kbd "C-x n h") 'git-gutter:next-hunk)
;;   (global-set-key (kbd "C-x n p") 'git-gutter:previous-hunk)
;;   (global-set-key (kbd "C-x n v =") 'git-gutter:popup-hunk)
;;   (global-set-key (kbd "C-x n r") 'git-gutter:revert-hunk)
;;   (global-set-key (kbd "C-x n t") 'git-gutter:stage-hunk))


;; ;;; git-svn support
;;
;; (leaf magit-svn :ensure t)
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


(leaf git-messenger
  :ensure t
  :config
  (global-set-key (kbd "C-x v p") #'git-messenger:popup-message))



(provide 'init-git)
;;; init-git.el ends here
