;;; init-python-mode --- python-mode configuration
;;; Commentary:
;;; Code:
(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))


;;; Jedi
(require-package 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
;; (setq ac-sources
;;       (delete 'ac-source-words-in-same-mode-buffers ac-sources))
;; (add-to-list 'ac-sources 'ac-source-filename)
;; (add-to-list 'ac-sources 'ac-source-jedi-direct)
;; (define-key jedi-mode-map (kbd "<C-tab>") nil)


;;; Py-Autopep8
(require-package 'py-autopep8)
(setq py-autopep8-options '("--max-line-length=200"))
(setq flycheck-flake8-maximum-line-length 200)
(py-autopep8-enable-on-save)


;;; PyFlakes
(unless (require 'flymake-python-pyflakes nil 'noerror)
  (el-get-bundle purcell/flymake-python-pyflakes))
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)


(provide 'init-python-mode)
;;; init-python-mode.el ends here
