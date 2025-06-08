;;; init-irc -- irc configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(unless (featurep 'leaf)
  (require 'init-leaf))
;; ;;; IRC
;; (leaf circe :ensure t)


;;; Twitter
(leaf twittering-mode :ensure t)


;;; Chatwork
;; (leaf chatwork :ensure t)
;; (setq chatwork-token "YOUR CHATWORK API TOKEN")


(provide 'init-irc)
;;; init-irc.el ends here
