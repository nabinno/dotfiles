;;; init-haskell --- haskell configuration
;;; Commentary:
;;; Code:
(use-package haskell-mode
  :straight t
  :config
  (custom-set-variables
   '(haskell-process-type 'cabal-repl)
   '(haskell-process-args-cabal-repl
     '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
   '(haskell-notify-p t)
   '(haskell-stylish-on-save nil)
   ;; '(haskell-stylish-on-save t)
   '(haskell-tags-on-save nil)
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)
   '(haskell-process-reload-with-fbytecode nil)
   '(haskell-process-use-presentation-mode t)
   '(haskell-interactive-mode-include-file-name nil)
   '(haskell-interactive-mode-eval-pretty nil)
   '(shm-use-presentation-mode t)
   '(shm-auto-insert-skeletons t)
   '(shm-auto-insert-bangs t)
   '(haskell-process-suggest-haskell-docs-imports t)
   '(hindent-style "chris-done")
   '(haskell-interactive-mode-eval-mode 'haskell-mode)
   '(haskell-process-path-ghci "ghci-ng")
   '(haskell-process-args-ghci '("-ferror-spans"))
   '(haskell-process-args-cabal-repl
     '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
   '(haskell-process-generate-tags nil)
   '(haskell-complete-module-preferred
     '("Data.ByteString"
       "Data.ByteString.Lazy"
       "Data.Conduit"
       "Data.Function"
       "Data.List"
       "Data.Map"
       "Data.Maybe"
       "Data.Monoid"
       "Data.Ord"))))


;; Flycheck specifics
(when (> emacs-major-version 23)
  (use-package flycheck-hdevtools :straight t)
  (use-package flycheck-haskell :straight t)
  (after-load 'flycheck
    (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

    (defun sanityinc/flycheck-haskell-reconfigure ()
      "Reconfigure flycheck haskell settings, e.g. after changing cabal file."
      (interactive)
      (unless (eq major-mode 'haskell-mode)
        (error "Expected to be in haskell-mode"))
      (flycheck-haskell-clear-config-cache)
      (flycheck-haskell-configure)
      (flycheck-mode -1)
      (flycheck-mode))

    (defadvice haskell-mode-stylish-buffer (around skip-if-flycheck-errors activate)
      "Don't run stylish-buffer if the buffer appears to have a syntax error.
This isn't a hard guarantee, since flycheck might sometimes not run until the file has
been saved."
      (unless (flycheck-has-current-errors-p 'error)
        ad-do-it))

    (require 'flycheck-hdevtools)))


;; Language Server Protocol
(use-package lsp-haskell :straight t)


;; Interactive hasskell mode
(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode)
  (add-hook hook (lambda () (subword-mode +1))))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(after-load 'haskell-interactive-mode
  (diminish 'interactive-haskell-mode " IntHS"))

;; Files
(add-auto-mode 'haskell-mode "\\.ghci\\'")


;;; Hi2
(use-package hi2
  :straight t
  :config
  (add-hook 'haskell-mode-hook 'turn-on-hi2))


;;; Structured haskell mode (like Paredit)
(unless (require 'structured-haskell-mode nil 'noerror)
  (el-get-bundle projectional-haskell/structured-haskell-mode))
(add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)


;;; Stack IDE
(unless (require 'stack-ide nil 'noerror)
  (el-get-bundle commercialhaskell/stack-ide))
(add-hook 'haskell-mode-hook 'stack-mode)


;;; Other functions
(defun haskell-process-all-types ()
  "List all types in a grep-mode buffer."
  (interactive)
  (let ((session (haskell-session)))
    (switch-to-buffer (get-buffer-create (format "*%s:all-types*"
                                                 (haskell-session-name (haskell-session)))))
    (setq haskell-session session)
    (cd (haskell-session-current-dir session))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((haskell-process-log nil))
        (insert (haskell-process-queue-sync-request (haskell-process) ":all-types")))
      (unless (eq major-mode  'compilation-mode)
        (compilation-mode)
        (setq compilation-error-regexp-alist
              haskell-compilation-error-regexp-alist)))))

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (insert "undefined"))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

;; (defun haskell-auto-insert-module-template ()
;;   "Insert a module template for the newly created buffer."
;;   (interactive)
;;   (when (and (= (point-min)
;;                 (point-max))
;;              (buffer-file-name))
;;     (insert
;;      "-- | "
;;      "\n"
;;      "\n"
;;      "module ")
;;     (let ((name (haskell-guess-module-name)))
;;       (if (string= name "")
;;           (progn (insert "Main")
;;                  (shm-evaporate (- (point) 5)
;;                                 (point)))
;;         (insert name)))
;;     (insert " where"
;;             "\n"
;;             "\n")
;;     (goto-char (point-min))
;;     (forward-char 4)))

;; Hooks
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;; Keybinds
(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-o") 'open-line)
  (define-key haskell-mode-map (kbd "C-c i") 'hindent/reformat-decl)
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)
  (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
  (define-key haskell-mode-map (kbd "C-<right>") 'haskell-move-right)
  (define-key haskell-mode-map (kbd "C-<left>") 'haskell-move-left)
  )

(eval-after-load 'page-break-lines
  '(push 'haskell-mode page-break-lines-modes))


;;; Compilation
(when (eval-when-compile (>= emacs-major-version 24))
  (use-package ghci-completion :straight t)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))


(provide 'init-haskell)
;;; init-haskell.el ends here
