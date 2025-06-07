;;; init-straight --- Configure straight.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	  (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package
(straight-use-package 'use-package)
(use-package use-package-ensure-system-package :straight t)


;; base
(defun jjin/use-package-if-prehook (name _keyword pred rest state)
  (unless pred (error "predicated failed; skipping package")))
(advice-add 'use-package-handler/:if :before 'jjin/use-package-if-prehook)

(use-package diminish :straight t)
(use-package names :straight t)
(use-package system-packages
  :straight t
  :init
  (setq system-packages-use-sudo nil)
  (when (eq system-type 'darwin)
    (setq system-packages-package-manager 'brew)))



(provide 'init-straight)
;;; init-straight.el ends here
