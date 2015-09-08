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


;;; Terraform
(require-package 'terraform-mode)
(setq terraform-indent-level 2)

(defun terraform (arg)
  "Terraform task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*terraform*")))
    (apply 'make-comint-in-buffer "terraform" buffer "terraform" nil (list arg))))


;;; AWS
(require-package 'helm-aws)

(defun aws-ec2 (arg)
  "Amazon EC2 task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<ec2>*")))
    (apply 'make-comint-in-buffer "aws-ec2" buffer "~/.emcs.d/bin/aws_ec2" nil (list arg))))
(defun aws-ecs (arg)
  "Amazon ECS task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<ecs>*")))
    (apply 'make-comint-in-buffer "aws-ecs" buffer "~/.emcs.d/bin/aws_ecs" nil (list arg))))
(defun aws-s3 (arg)
  "Amazon S3 task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<s3>*")))
    (apply 'make-comint-in-buffer "aws-s3" buffer "~/.emcs.d/bin/aws_s3" nil (list arg))))
(defun aws-route53 (arg)
  "Amazon Route53 task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<route53>*")))
    (apply 'make-comint-in-buffer "aws-route53" buffer "~/.emcs.d/bin/aws_route53" nil (list arg))))


;;; Heroku
(require-package 'heroku)



(provide 'init-orchestration)
;;; init-orchestration.el ends here
