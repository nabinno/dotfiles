;;; Win switch
(require-package 'win-switch)

(setq win-switch-idle-time 0.75)
;; change window
(win-switch-set-keys '("k") 'up)
(win-switch-set-keys '("j") 'down)
(win-switch-set-keys '("h") 'left)
(win-switch-set-keys '("l") 'right)
(win-switch-set-keys '("o") 'next-window)
(win-switch-set-keys '("p") 'previous-window)
;; resize
(win-switch-set-keys '("K") 'enlarge-vertically)
(win-switch-set-keys '("J") 'shrink-vertically)
(win-switch-set-keys '("H") 'shrink-horizontally)
(win-switch-set-keys '("L") 'enlarge-horizontally)
;; split
(win-switch-set-keys '("3") 'split-horizontally)
(win-switch-set-keys '("2") 'split-vertically)
(win-switch-set-keys '("0") 'delete-window)
;; other
(win-switch-set-keys '(" ") 'other-frame)
(win-switch-set-keys '("u" [return]) 'exit)
(win-switch-set-keys '("\M-\C-g") 'emergency-exit)
;; global set key
(global-set-key (kbd "M-[ 1 ; 8 a") 'win-switch-up)
(global-set-key (kbd "M-[ 1 ; 8 b") 'win-switch-down)
(global-set-key (kbd "M-[ 1 ; 8 c") 'win-switch-right)
(global-set-key (kbd "M-[ 1 ; 8 d") 'win-switch-left)
;; (global-set-key (kbd "M-[ 1 ; 4 a") 'shrink-window)
;; (global-set-key (kbd "M-[ 1 ; 4 b") 'enlarge-window)
(global-set-key (kbd "M-[ 1 ; 4 c") 'shrink-window-horizontally)
(global-set-key (kbd "M-[ 1 ; 4 d") 'enlarge-window-horizontally)
(global-set-key (kbd "M-") 'delete-window)
(global-set-key (kbd "S-C-M-q") 'delete-window)
(global-set-key (kbd "M-q") 'delete-window)


;;; Switch window
(require-package 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
(global-set-key (kbd "M-[ 1 ; 6 i") 'switch-window)
(global-set-key (kbd "M-i") 'switch-window)


;;; OWDriver
(require-package 'owdriver)

(require 'owdriver)
(global-unset-key (kbd "M-o"))
(setq owdriver-prefix-key "M-o")
(owdriver-config-default)
(owdriver-add-keymap owdriver-prefix-key 'owdriver-next-window)
(owdriver-mode 1)


(provide 'init-other-window)
