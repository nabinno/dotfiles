;;; init-ocaml --- ocaml configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Tuareg
(leaf tuareg :el-get ocaml/tuareg)


;; Language Server Protocol and Merlin as IDE
;; (use-package lsp-ocaml
;;   :straight (:host github :repo "emacs-lsp/lsp-ocaml"))
(leaf merlin :el-get ocaml/merlin)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'tuareg-mode-hook #'lsp-ocaml-enable)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook #'lsp-ocaml-enable)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))


;; Base configuration for OPAM
(defun opam-shell-command-to-string (command)
  "Similar to `shell-command-to-string' with COMMAND.
But return nil unless the process returned 0 (`shell-command-to-string' ignore return value)."
  (let* ((return-value 0)
         (return-string
          (with-output-to-string
            (setq return-value
                  (with-current-buffer standard-output
                    (process-file shell-file-name nil t nil
                                  shell-command-switch command))))))
    (if (= return-value 0) return-string nil)))

(defun opam-update-env (switch)
  "Update the environment to follow current opam SWITCH configuration."
  (interactive "sopam switch (empty to keep current setting): ")
  (let* ((switch-arg (if (= 0 (length switch)) "" (concat "--switch " switch)))
         (command (concat "opam config env --sexp " switch-arg))
         (env (opam-shell-command-to-string command)))
    (when env
      (dolist (var (car (read-from-string env)))
        (setenv (car var) (cadr var))
        (when (string= (car var) "PATH")
          (setq exec-path (split-string (cadr var) path-separator)))))))

(opam-update-env nil)

(setq opam-share
      (let ((reply (opam-shell-command-to-string "opam config var share")))
        (when reply (substring reply 0 -1))))

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; OPAM-installed tools automated detection and initialisation

(defun opam-setup-tuareg ()
  (add-to-list 'load-path (concat opam-share "/tuareg") t)
  (load "tuareg-site-file"))

(defun opam-setup-add-ocaml-hook (h)
  (add-hook 'tuareg-mode-hook h t)
  (add-hook 'caml-mode-hook h t))

(defun opam-setup-complete ()
  (if (require 'company nil t)
    (opam-setup-add-ocaml-hook
      (lambda ()
  (company-mode)
  (defalias 'auto-complete 'company-complete)))
    (require 'auto-complete nil t)))

(defun opam-setup-ocp-indent ()
  (opam-setup-complete)
  (autoload 'ocp-setup-indent "ocp-indent" "Improved indentation for Tuareg mode")
  (autoload 'ocp-indent-caml-mode-setup "ocp-indent" "Improved indentation for Caml mode")
  (add-hook 'tuareg-mode-hook 'ocp-setup-indent t)
  (add-hook 'caml-mode-hook 'ocp-indent-caml-mode-setup  t))

(defun opam-setup-ocp-index ()
  (autoload 'ocp-index-mode "ocp-index" "OCaml code browsing, documentation and completion based on build artefacts")
  (opam-setup-add-ocaml-hook 'ocp-index-mode))

(defun opam-setup-merlin ()
  (opam-setup-complete)
  (require 'merlin)
  (opam-setup-add-ocaml-hook 'merlin-mode)

  (defcustom ocp-index-use-auto-complete nil
  "Use auto-complete with ocp-index (disabled by default by opam-user-setup because merlin is in use)"
  :group 'ocp_index)
  (defcustom merlin-ac-setup 'easy
  "Use auto-complete with merlin (enabled by default by opam-user-setup)"
  :group 'merlin-ac)

  ;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
  ;; by spaces.
  (define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
  (define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
  (set-face-background 'merlin-type-face "skyblue"))

(defun opam-setup-utop ()
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode))

(setq opam-tools
      '(("tuareg" . opam-setup-tuareg)
        ("ocp-indent" . opam-setup-ocp-indent)
        ("ocp-index" . opam-setup-ocp-index)
        ("merlin" . opam-setup-merlin)
        ("utop" . opam-setup-utop)))

(defun opam-detect-installed-tools ()
  (let*
    ((command "opam list --installed --short --safe --color=never")
       (names (mapcar 'car opam-tools))
       (command-string (mapconcat 'identity (cons command names) " "))
       (reply (opam-shell-command-to-string command-string)))
    (when reply (split-string reply))))

(setq opam-tools-installed (opam-detect-installed-tools))

(defun opam-auto-tools-setup ()
  (interactive)
  (dolist (tool opam-tools)
    (when (member (car tool) opam-tools-installed)
     (funcall (symbol-function (cdr tool))))))

(opam-auto-tools-setup)


;; ReasonML as AltJS
(leaf reason-mode :el-get reasonml-editor/reason-mode)

(setq refmt-width-mode 'fill)

(defun shell-cmd (command)
  "Return the stdout output of a shell COMMAND or nil if the command returned an error."
    (car (ignore-errors (apply 'process-lines (split-string command)))))

(let* ((refmt-bin (or (shell-cmd "refmt ----where")
                      (shell-cmd "which refmt")))
       (merlin-bin (or (shell-cmd "ocamlmerlin ----where")
                       (shell-cmd "which ocamlmerlin")))
       (merlin-base-dir (when merlin-bin
                          (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
  ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
  (when merlin-bin
    (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
    (setq merlin-command merlin-bin))
  (when refmt-bin
    (setq refmt-command refmt-bin)))

(add-hook 'reason-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'refmt-before-save)
            (add-hook 'reason-mode-hook #'lsp-ocaml-enable)
            (merlin-mode)))


(provide 'init-ocaml)
;;; init-ocaml.el ends here
