;;; init-marmalade --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(leaf marmalade :el-get nicferrier/emacs-marmalade-upload)

;;; Handy code for uploading new versions of my own packages to marmalade

(autoload 'marmalade-upload-buffer "marmalade")

(defun sanityinc/parse-git-version (s)
  "Return numeric version array parsed from S, or nil."
  (ignore-errors (version-to-list s)))

(defun latest-version-from-git-tag ()
  (let ((versions
         (remove-if #'null
                    (mapcar #'sanityinc/parse-git-version
                            (split-string (shell-command-to-string "git tag"))))))
    (sort versions #'version-list-<)
    (package-version-join (car (last versions)))))

(defun update-version-header (val)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^;;;? ?Version:")
    (kill-line)
    (insert " " val)))

(defun submit-tar-to-marmalade (buf)
  (interactive "bSubmit buffer library as tar: ")
  (with-current-buffer buf
    (let* ((tag (or (latest-version-from-git-tag) (error "Not tagged")))
           (library-name (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
           (package-dir-name (concat library-name "-" tag))
           (temp-working-dir (make-temp-file "emacs-marmalade" t))
           (dest (expand-file-name package-dir-name temp-working-dir))
           (tar-cmd (or (executable-find "gtar")
                        (executable-find "tar")))
           (tar (concat dest ".tar")))
      (message "Building package in %s" dest)
      (make-directory dest)
      (let ((command-line (format "cp *.el %s && (cd %s && perl -spi -e 's/\\{\\{VERSION\\}\\}/%s/' *.el) && (cd %s && %s cvf %s %s)" dest dest tag temp-working-dir tar-cmd tar package-dir-name)))
        (shell-command command-line))
      (save-excursion
        (shell-command (format "open %s" temp-working-dir))
        ;; (find-file tar)
        ;; (marmalade-upload-buffer (current-buffer))
;;      (delete-directory temp-working-dir t)
      ))))

(defun submit-to-marmalade (buf)
  "Submit the elisp library in BUF to Marmalade."
  (interactive
   (list
    (let ((buffers (loop for b in (mapcar 'buffer-name (buffer-list))
                         when (with-current-buffer b
                                (and buffer-file-name
                                     (eq major-mode 'emacs-lisp-mode)))
                         collect b)))
      (completing-read "Submit buffer: " buffers nil t nil nil (car buffers)))))
  (with-current-buffer buf
    (let ((tag (latest-version-from-git-tag)))
      (unless tag
        (error "Not tagged"))
      (update-version-header tag)
      (marmalade-upload-buffer buf)
      (revert-buffer t t)
      (message "Submitted version %s to marmalade" tag))))



(provide 'init-marmalade)
;;; init-marmalade.el ends here
