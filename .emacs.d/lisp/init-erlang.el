;;; init-erlang --- erlang configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
(leaf erlang
  :ensure t
  :if (package-installed-p 'erlang)
  :config
  ;; (add-to-list 'ac-modes 'erlang-mode)
  )


;;; Exlixr
(leaf elixir-mode
  :ensure t
  :config
  (setq auto-mode-alist
        (cons '("\\.\\(po\\|pot\\)\\'" . elixir-mode) auto-mode-alist)))

;; alchemist
(leaf alchemist-mode
  :el-get tonini/alchemist.el
  :config
  (setq alchemist-key-command-prefix (kbd "C-c ,")) ;; default: (kbd "C-c a")
  (setq alchemist-goto-erlang-source-dir "~/.local/erlang/")
  (setq alchemist-goto-elixir-source-dir "~/.local/elixir/")
  (defun custom-erlang-mode-hook ()
    "Define key to erlang-mode-map."
    (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
  (defun custom-alchemist-mode-hook ()
    "Define key to alchemist-mode-map."
    (define-key alchemist-mode-map (kbd "M-,") 'mc/mark-previous-like-this)
    (define-key alchemist-mode-map (kbd "M-.") 'mc/mark-next-like-this))
  (defadvice alchemist-project-root (around seancribbs/alchemist-project-root activate)
    "Advice alchemist-project-mix-project-indicator."
    (let ((alchemist-project-mix-project-indicator ".git"))
      ad-do-it))
  (defun seancribbs/activate-alchemist-root-advice ()
    "Activates advice to override alchemist's root-finding logic."
    (ad-activate 'alchemist-project-root))
  (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
  (add-hook 'alchemist-mode-hook 'custom-alchemist-mode-hook)
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (add-hook 'elixir-mode-hook 'seancribbs/activate-alchemist-root-advice))

;; mix-format
(defun mix-format-after-save ()
  "Run `mix format' in Emacs."
  (if (and (derived-mode-p 'elixir-mode)
           (not (or (equal (file-name-extension (buffer-file-name)) "po")
                    (equal (file-name-extension (buffer-file-name)) "pot"))))
      (shell-command (concat "mix format " (buffer-file-name)))))
(add-hook 'after-save-hook 'mix-format-after-save)


;;; Phoenix
(leaf xinari
  :el-get nabinno/xinari
  :config
  (after-load 'xinari
    (diminish 'xinari-minor-mode "Xin"))
  (global-xinari-mode))
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
