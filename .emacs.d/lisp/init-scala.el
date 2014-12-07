
;;; Basic scala setup
You should have a section marked ";;; Commentary:"
The first line should be of the form: ";;; package --- Summary"
(require-package 'scala-mode2)
(add-auto-mode 'scala-mode "\\.scala\\'")
(add-hook 'scala-mode-hook
          '(lambda () (scala-mode-feature-electric-mode)))


;;; SBT
(require-package 'sbt-mode)
;; (defun ensime-sbt-project-dir-p (path)
;;   "Is path an sbt project?"
;;   (or (not (null (directory-files path nil "\\.sbt$")))
;;       (file-exists-p (concat path "/project/Build.scala" ))
;;       (file-exists-p (concat path "/project/boot" ))
;;       (file-exists-p (concat path "/project/build.properties" ))))


;;; Ensime
(require-package 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(provide 'init-scala)
