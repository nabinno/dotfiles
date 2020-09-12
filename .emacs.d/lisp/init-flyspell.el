;;; init-flyspell --- basic flyspell configuration
;;; Commentary:
;;; Code:

;;; Add spell-checking in comments for all programming language modes
(if (fboundp 'prog-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (dolist (hook '(caml-mode-hook
                  clojure-mode-hook
                  crontab-mode-hook
                  css-mode-hook
                  emacs-lisp-mode-hook
                  haskell-mode-hook
                  javascript-mode-hook
                  js-mode-hook
                  js2-mode-hook
                  lisp-mode-hook
                  nxml-mode-hook
                  perl-mode-hook
                  php-mode-hook
                  python-mode-hook
                  ruby-mode-hook
                  scheme-mode-hook
                  shell-mode-hook
                  tcl-mode-hook
                  yaml-mode))
    (add-hook hook 'flyspell-prog-mode)))

(after-load 'flyspell
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))


;;; keybind
(global-set-key (kbd"C-c <f8>") 'flyspell-mode)
(global-set-key (kbd"C-c <f9>") 'ispell-word)
(global-set-key (kbd"C-c <f10>") 'flyspell-buffer)


;;; available for comment area
(mapc (lambda (hook) (add-hook hook'flyspell-prog-mode))
      '(c-mode-common-hook
        ruby-mode-hook
        emacs-lisp-mode-hook))
(mapc (lambda (hook) (add-hook hook '(lambda () (flyspell-mode1))))
      '(text-mode-hook
        org-mode-hook
        wl-draft-mode-hook
        twittering-edit-mode))


;; flyspell-correct
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct
  :config
  (setq flyspell-correct-interface '#'flyspell-correct-ivy)
  (global-set-key (kbd "<f7>") 'flyspell-correct-word-generic))



(provide 'init-flyspell)
;;; init-flyspell.el ends here.




