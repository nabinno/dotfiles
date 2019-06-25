;;; init-elscreen -- elscreen configuration
;;; Commentary:
;;; Code:
(require-package 'elscreen)

;; (setq elscreen-prefix-key (kbd "M-[ 1 ; 5 q"))

(setq elscreen-display-tab nil)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$"     . (lambda () (format "Dired(%s)" dired-directory)))
        ("^Info-mode$"      . (lambda () (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" . (lambda () (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-"            . "Mew")
        ("^irchat-"         . "IRChat")
        ("^liece-"          . "Liece")
        ("^lookup-"         . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell"     . "shell")
        ("compilation"  . "compile")
        ("-telnet"      . "telnet")
        ("dict"         . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; keybind
(global-set-key (kbd "M-)") '(lambda () (interactive) (elscreen-goto 0)))
(global-set-key (kbd "M-!") '(lambda () (interactive) (elscreen-goto 1)))
(global-set-key (kbd "M-@") '(lambda () (interactive) (elscreen-goto 2)))
(global-set-key (kbd "M-#") '(lambda () (interactive) (elscreen-goto 3)))
(global-set-key (kbd "M-$") '(lambda () (interactive) (elscreen-goto 4)))
(global-set-key (kbd "M-%") '(lambda () (interactive) (elscreen-goto 5)))
(global-set-key (kbd "M-^") '(lambda () (interactive) (elscreen-goto 6)))
(global-set-key (kbd "M-&") '(lambda () (interactive) (elscreen-goto 7)))
(global-set-key (kbd "M-*") '(lambda () (interactive) (elscreen-goto 8)))
(global-set-key (kbd "M-(") '(lambda () (interactive) (elscreen-goto 9)))
(global-set-key (kbd "C-M-)") '(lambda () (interactive) (elscreen-goto 0)))
(global-set-key (kbd "C-M-!") '(lambda () (interactive) (elscreen-goto 1)))
(global-set-key (kbd "C-M-@") '(lambda () (interactive) (elscreen-goto 2)))
(global-set-key (kbd "C-M-#") '(lambda () (interactive) (elscreen-goto 3)))
(global-set-key (kbd "C-M-$") '(lambda () (interactive) (elscreen-goto 4)))
(global-set-key (kbd "C-M-%") '(lambda () (interactive) (elscreen-goto 5)))
(global-set-key (kbd "C-M-^") '(lambda () (interactive) (elscreen-goto 6)))
(global-set-key (kbd "C-M-&") '(lambda () (interactive) (elscreen-goto 7)))
(global-set-key (kbd "C-M-*") '(lambda () (interactive) (elscreen-goto 8)))
(global-set-key (kbd "C-M-(") '(lambda () (interactive) (elscreen-goto 9)))
(global-set-key (kbd "C-c 0") '(lambda () (interactive) (elscreen-goto 0)))
(global-set-key (kbd "C-c 1") '(lambda () (interactive) (elscreen-goto 1)))
(global-set-key (kbd "C-c 2") '(lambda () (interactive) (elscreen-goto 2)))
(global-set-key (kbd "C-c 3") '(lambda () (interactive) (elscreen-goto 3)))
(global-set-key (kbd "C-c 4") '(lambda () (interactive) (elscreen-goto 4)))
(global-set-key (kbd "C-c 5") '(lambda () (interactive) (elscreen-goto 5)))
(global-set-key (kbd "C-c 6") '(lambda () (interactive) (elscreen-goto 6)))
(global-set-key (kbd "C-c 7") '(lambda () (interactive) (elscreen-goto 7)))
(global-set-key (kbd "C-c 8") '(lambda () (interactive) (elscreen-goto 8)))
(global-set-key (kbd "C-c 9") '(lambda () (interactive) (elscreen-goto 9)))
;; (global-set-key (kbd "M-[ 1 ; 5 h") 'elscreen-previous)
;; (global-set-key (kbd "M-[ 1 ; 5 f") 'elscreen-next)
;; (global-set-key (kbd "M-[ 1 ; 8 p") 'elscreen-jump-0)
;; (global-set-key (kbd "M-[ 1 ; 8 q") '(lambda () (interactive) (elscreen-goto 1)))
;; (global-set-key (kbd "M-[ 1 ; 8 s") '(lambda () (interactive) (elscreen-goto 3)))
;; (global-set-key (kbd "M-[ 1 ; 8 t") '(lambda () (interactive) (elscreen-goto 4)))
;; (global-set-key (kbd "M-[ 1 ; 8 u") '(lambda () (interactive) (elscreen-goto 5)))
;; (global-set-key (kbd "M-[ 1 ; 8 w") '(lambda () (interactive) (elscreen-goto 7)))
;; (global-set-key (kbd "M-[ 1 ; 8 x") '(lambda () (interactive) (elscreen-goto 8)))
;; (global-set-key (kbd "M-[ 1 ; 8 y") 'elscreen-jump-9)
;; (global-set-key (kbd "<f50>") 'elscreen-create)
;; (global-set-key (kbd "ESC <f2>") 'elscreen-create)
;; (global-set-key (kbd "M-[ 1 ; 3 Q") 'elscreen-create)
(global-set-key (kbd "<f2>") 'elscreen-create)

(elscreen-start)


;; ;;; Elscreen persist
;; (unless (require 'elscreen-persist nil 'noerror)
;;   (el-get-bundle robario/elscreen-persist))
;; ;; (elscreen-persist-mode 1)


;;; Elscreen separate buffer list
(require-package 'elscreen-separate-buffer-list)
(elscreen-separate-buffer-list-mode)



(provide 'init-elscreen)
;;; init-elscreen.el ends here
