;;; init-orchestration --- orchestration configuration
;;; Commentary:
;;; Code:

;;; OS level virtualization
;; vagrant
(require-package 'vagrant)

;; docker
(require-package 'docker)
(require-package 'docker-tramp)
;; (require-package 'docker-file-mode)
(require-package 'marcopolo)


;;; Platform service
;; terraform
(require-package 'terraform-mode)
(setq terraform-indent-level 2)

;; ;; amazon ec2
;; (require-package 'helm-aws)

;; heroku
(require-package 'heroku)



(provide 'init-orchestration)
;;; init-orchestration.el ends here
