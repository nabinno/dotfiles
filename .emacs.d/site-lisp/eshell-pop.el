(require 'shell-pop)
(shell-pop-set-internal-mode "eshell")
(defadvice shell-pop-up (around eshell-cd-default-directory activate)
  (when (get-buffer "*eshell*")
    (with-current-buffer "*eshell*"
      (when (eshell-interactive-process)
        (rename-uniquely))))
  (let ((dir default-directory))
    ad-do-it
    (unless (equal dir default-directory)
      (eshell-interactive-print (concat "pushd " dir "\n"))
      (eshell/pushd dir)
      (eshell-emit-prompt))
    (goto-char (point-max))))

;; shell-pop-last-window
(defadvice eshell-life-is-too-much (after shell-pop activate)
  (shell-pop-out))

(provide 'eshell-pop)
