;;; init-project -- project configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Sr Speedbar
(leaf sr-speedbar :ensure t)
(leaf sr-speedbar
  :ensure t
  :config
  (global-set-key (kbd "M-3") 'sr-speedbar-toggle)
  (setq speedbar-hide-button-brackets-flag t
        speedbar-show-unknown-files t
        speedbar-smart-directory-expand-flag t
        speedbar-use-images nil
        speedbar-indentation-width 1
        speedbar-update-flag t
        sr-speedbar-width 20
        sr-speedbar-width-x 20
        sr-speedbar-auto-refresh t
        sr-speedbar-skip-other-window-p t
        sr-speedbar-right-side t)

  ;; keybind
  (define-key speedbar-mode-map (kbd "p")  'speedbar-backward-list)
  (define-key speedbar-mode-map (kbd "o")  'speedbar-toggle-line-expansion)
  (define-key speedbar-mode-map (kbd "+")  'speedbar-create-directory)
  (define-key speedbar-mode-map (kbd "i")  'speedbar-expand-line-descendants)
  (define-key speedbar-mode-map (kbd "c")  'speedbar-item-copy)
  (define-key speedbar-mode-map (kbd "d")  'speedbar-item-delete)
  (define-key speedbar-mode-map (kbd "?")  'speedbar-item-info)
  (define-key speedbar-mode-map (kbd "r")  'speedbar-item-rename)
  (define-key speedbar-mode-map (kbd "j")  'speedbar-next)
  (define-key speedbar-mode-map (kbd "k")  'speedbar-prev)
  (define-key speedbar-mode-map (kbd "g")  'speedbar-refresh)
  (define-key speedbar-mode-map (kbd "J")  'speedbar-restricted-next)
  (define-key speedbar-mode-map (kbd "K")  'speedbar-restricted-prev)
  (define-key speedbar-mode-map (kbd "u")  'speedbar-scroll-down)
  (define-key speedbar-mode-map (kbd "b")  'speedbar-scroll-up)
  (define-key speedbar-mode-map (kbd "s")  'speedbar-toggle-sorting)
  (define-key speedbar-mode-map (kbd "") 'speedbar-up-directory)
  (define-key speedbar-mode-map (kbd "f")  'isearch-forward)
  (define-key speedbar-mode-map (kbd "l")  'recenter)
  (define-key speedbar-mode-map (kbd "h")   (lambda () (interactive) (progn (speedbar-edit-line))))
  (define-key speedbar-mode-map (kbd "m")   (lambda () (interactive) (progn (speedbar-next 1) (speedbar-edit-line))))
  (define-key speedbar-mode-map (kbd "SPC") (lambda () (interactive) (progn (speedbar-next 1) (speedbar-edit-line))))
  (define-key speedbar-mode-map (kbd "p")   (lambda () (interactive) (progn (goto-char (point-min)) (speedbar-next 1))))
  (define-key speedbar-mode-map (kbd "n")   (lambda () (interactive) (progn (goto-char (point-max)) (speedbar-prev 1))))
  )



;; ;;; Neotree
;; (leaf neotree :ensure t)
;; (when (maybe-require-package 'neotree)
;;   (require 'neotree)
;;   (global-set-key (kbd "M-3") 'neotree-toggle))

;; (add-hook 'neotree-mode-hook
;;           (lambda ()
;;            (define-key neotree-mode-map (kbd "i") 'neotree-enter)
;;            (define-key neotree-mode-map (kbd "j") 'neotree-next-line)
;;            (define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
;;            (define-key neotree-mode-map (kbd "K") 'neotree-create-node)
;;            (define-key neotree-mode-map (kbd "c") 'neotree-copy-node)
;;            (define-key neotree-mode-map (kbd "d") 'neotree-delete-node)
;;            (define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
;;            (define-key neotree-mode-map (kbd "f") 'isearch-forward)
;;            (define-key neotree-mode-map (kbd "p") 'beginning-of-buffer)
;;            (define-key neotree-mode-map (kbd "n") 'end-of-buffer))
;;           )


;;; Task management
;; todo hilight
(dolist (mode '(ruby js js2 terraform cperl elixir erlang python jade haml emacs-lisp ielm red go))
  (progn
    (font-lock-add-keywords
     (intern (format "%s-mode" mode))
     '(("\\<\\(FIX\\|TODO\\|DONE\\|FIXME\\|HACK\\|REFACTOR\\):"
        1 font-lock-warning-face t)))
    (font-lock-add-keywords
     (intern (format "%s-mode" mode))
     '((" \\(@\\(todo\\|desc\\|param\\|note\\|see\\|type\\|return\\|fixme\\|example\\|raise\\)\\)"
        1 font-lock-warning-face t)))))

;; (leaf fic-mode :ensure t)
;; (when (maybe-require-package 'fic-mode) (require 'fic-mode))
;; ;; (leaf hl-todo :ensure t)
;; ;; (when (maybe-require-package 'hl-todo) (require 'hl-todo))
;; (defun insert-todo-mark () (interactive)
;;            (insert (shell-command-to-string "echo -n TODO: $(date +%Y-%m-%d)")))




(provide 'init-project)

;; (leaf pomodoro :ensure t)




(provide 'init-project)
;;; init-project.el ends here
