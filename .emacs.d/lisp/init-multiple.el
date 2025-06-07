;;; init-multiple --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------

(require-package 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)


;; ;; WebMode
;; (el-get-bundle fxbois/web-mode)
;; ;; (require-package 'web-mode)
;; (add-to-list 'auto-mode-alist'("\\.phtml\\'" .phtmlweb-mode))
;; (add-to-list 'auto-mode-alist'("\\.tpl\\.php\\'" .web-mode))
;; (add-to-list 'auto-mode-alist'("\\.[agj]sp\\'" .web-mode))
;; (add-to-list 'auto-mode-alist'("\\.as[cp]x\\'" .web-mode))
;; (add-to-list 'auto-mode-alist'("\\.erb\\'" .web-mode))
;; (add-to-list 'auto-mode-alist'("\\.eex\\'" .web-mode))
;; (add-to-list 'auto-mode-alist'("\\.mustache\\'" .web-mode))
;; (add-to-list 'auto-mode-alist'("\\.djhtml\\'" .web-mode))



(provide 'init-multiple)
;;; init-multiple.el ends here
