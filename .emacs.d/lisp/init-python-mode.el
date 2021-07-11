;;; init-python-mode --- python-mode configuration
;;; Commentary:
;;; Code:
(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))


;; ;;; IPython
;; (setq python-shell-interpreter "ipython")
;; (let* ((profile-name "profile_for_emacs")
;;        (folder-name-getter `(substring (shell-command-to-string (concat "ipython locate profile " ,profile-name)) 0 -1)))
;;     (unless (file-directory-p (eval folder-name-getter))
;;       (shell-command (concat "ipython profile create " profile-name))
;;       (let ((ipython-config-folder (eval folder-name-getter)))
;;         (shell-command (concat "echo \"\n\nc.InteractiveShellApp.extensions.append(\\\"autoreload\\\")\nc.InteractiveShellApp.exec_lines.append(\\\"%autoreload 2\\\")\" >> " ipython-config-folder "/ipython_config.py"))))
;;     (set-variable 'python-shell-interpreter-args (concat "--profile=" profile-name " " python-shell-interpreter-args)))


;;; Language Server Protocol
(leaf lsp-python
  :after python
  :el-get emacs-lsp/lsp-python-ms)


;; ;;; Jedi
;; (leaf jedi :ensure t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
;; ;; (setq ac-sources
;; ;;       (delete 'ac-source-words-in-same-mode-buffers ac-sources))
;; ;; (add-to-list 'ac-sources 'ac-source-filename)
;; ;; (add-to-list 'ac-sources 'ac-source-jedi-direct)
;; ;; (define-key jedi-mode-map (kbd "<C-tab>") nil)


;;; Py-Autopep8
;; (leaf py-autopep8 :ensure t)
;; (setq py-autopep8-options '("--max-line-length=200"))
;; (setq flycheck-flake8-maximum-line-length 200)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;; Balcken
(leaf blacken
  :after python
  :ensure t)


;;; PyFlakes
(leaf flymake-python-pyflakes
  :el-get purcell/flymake-python-pyflakes
  :config
  (after-load 'flymake-python-pyflakes
    (flymake-python-pyflakes-load)))


(provide 'init-python-mode)
;;; init-python-mode.el ends here
