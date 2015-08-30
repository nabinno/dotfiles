;;; init-erlang --- erlang configuration
;;; Commentary:
;;; Code:
(ignore-errors
  (require-package 'erlang))

(when (package-installed-p 'erlang)
  (require 'erlang-start))

;; (add-to-list 'ac-modes 'erlang-mode)


;;; Exlixr
(require-package 'elixir-mode)

;; alchemist
(el-get-bundle tonini/alchemist.el)
(setq alchemist-key-command-prefix (kbd "C-c ,")) ;; default: (kbd "C-c a")

(setq alchemist-goto-erlang-source-dir "~/.parts/lib/erlang/")
(setq alchemist-goto-elixir-source-dir "~/.parts/lib/elixir/")
(defun custom-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
(add-hook 'elixir-mode-hook 'alchemist-mode)


;;; Phoenix
(unless (require 'xinari nil 'noerror)
  (el-get-bundle nabinno/xinari))

(after-load 'xinari
  (diminish 'xinari-minor-mode "Xin"))
(global-xinari-mode)

(defun update-express-ctags ()
  (interactive)
  (let ((default-directory (or (xinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " xinari-tags-file-name " --tag-relative -R src lib vendor test"))))

;; xinari-rgrep
(setq xinari-rgrep-file-endings "*.ex *.eex *.exs *.yml *.yaml *.coffee *.js *.es6 *.json *.scss *.rake")
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories ".old")
     (add-to-list 'grep-find-ignored-directories ".sass-cache")
     (add-to-list 'grep-find-ignored-directories "bin")
     (add-to-list 'grep-find-ignored-directories "bower_components")
     (add-to-list 'grep-find-ignored-directories "commons")
     (add-to-list 'grep-find-ignored-directories "commons.min")
     (add-to-list 'grep-find-ignored-directories "db")
     (add-to-list 'grep-find-ignored-directories "fonts")
     (add-to-list 'grep-find-ignored-directories "log")
     (add-to-list 'grep-find-ignored-directories "tmp")
     (add-to-list 'grep-find-ignored-directories "vendor")))



(provide 'init-erlang)
;;; init-erlang.el ends here
