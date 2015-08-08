(require-package 'elscreen)
(elscreen-start)

(setq elscreen-prefix-key (kbd "C-1"))
(setq elscreen-prefix-key (kbd "M-[ 1 ; 5 q"))
(setq elscreen-display-tab nil)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
                ("*WL:Message*" . "Wanderlust")))

(global-set-key (kbd "M-[ 1 ; 5 h") 'elscreen-previous)
(global-set-key (kbd "M-[ 1 ; 5 f") 'elscreen-next)


;;; Elscreen persist
(require-package 'elscreen-persist)
(elscreen-persist-mode 1)


;;; Elscreen separate buffer list
(require-package 'elscreen-separate-buffer-list)
(elscreen-separate-buffer-list-mode)



(provide 'init-elscreen)
