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


(provide 'init-flyspell)
