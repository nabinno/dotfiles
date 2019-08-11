;;; init-shell -- shell configuration
;;; Commentary:
;;; Code:
(setq sh-basic-offset 2)
(setq sh-indentation 2)


;; Shellcheck
(add-hook 'sh-mode-hook 'flycheck-mode)


;; Shfmat
(defun shfmt-after-save ()
  "Run `shfmt -i 2 -ci -w' in Emacs."
  (if (derived-mode-p 'sh-mode)
      (shell-command (concat "shfmt -i 2 -ci -w " (buffer-file-name)))))
(add-hook 'after-save-hook 'shfmt-after-save)


;; Other
(defun eval-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll
call 「python x.py」 in a shell.  The file can be Emacs Lisp,
PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic,
TeX, Java, Clojure.  File suffix is used to determine what
program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2016-01-28"
  (interactive)
  (let (
         (-suffix-map
          `(("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("go" . "go run")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("tex" . "pdflatex")
            ("latex" . "pdflatex")))
         -fname
         -fSuffix
         -prog-name
         -cmd-str)
    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq -fname (if (equal (file-name-extension (buffer-file-name)) "go")
                     "."
                   (buffer-file-name)))
    (setq -fSuffix (file-name-extension (buffer-file-name)))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))
    (if -prog-name
        (progn
          (message "Running...")
          (shell-command -cmd-str "*xah-run-current-file output*" ))
      (message "No recognized program file suffix for this file."))))
(global-set-key (kbd "C-x M-e") 'eval-current-file)



(provide 'init-shell)
;;; init-shell.el ends here
