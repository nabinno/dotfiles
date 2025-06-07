;;; init-orchestration --- orchestration configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; OS level virtualization
;; vagrant
(leaf vagrant :ensure t)

;; docker
(leaf docker :ensure t)
(leaf docker-tramp :ensure t)
(leaf dockerfile-mode :ensure t)
(leaf marcopolo :ensure t)


;;; Terraform
(leaf terraform-mode
  :ensure t
  :config
  (setq terraform-indent-level 2))

;; terraform command interpreter in a buffer
(defun terraform (arg)
  "Terraform task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*terraform*")))
    (apply 'make-comint-in-buffer "terraform" buffer "~/.emacs.d/bin/terraform" nil (list arg))))
(fset 'terraform-show                  "xterraformshow")
(fset 'terraform-show--module-depth--1 "xterraformshow -module-depth=-1")
(fset 'terraform-plan                  "xterraformplan")
(fset 'terraform-plan--module-depth--1 "xterraformplan -module-depth=-1")
(fset 'terraform-apply                 "xterraformapply")
(fset 'terraform-get                   "xterraformget")
(fset 'terraform-switch-buffer         "xido-switch-buffer*terraform*")
(global-set-key (kbd "\C-cts") 'terraform-show)
(global-set-key (kbd "\C-ctS") 'terraform-show--module-depth--1)
(global-set-key (kbd "\C-ctp") 'terraform-plan)
(global-set-key (kbd "\C-ctP") 'terraform-plan--module-depth--1)
(global-set-key (kbd "\C-cta") 'terraform-apply)
(global-set-key (kbd "\C-ctg") 'terraform-get)
(global-set-key (kbd "\C-ctt") 'terraform-switch-buffer)
(global-set-key (kbd "")   'terraform-switch-buffer)


;;; AWS
(leaf helm-aws :ensure t)

;; aws command interpreter in a buffer
(defun aws-ec2 (arg)
  "Amazon EC2 task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<ec2>*")))
    (apply 'make-comint-in-buffer "aws-ec2" buffer "~/.emacs.d/bin/aws_ec2" nil (list arg))))
(defun aws-ecs (arg)
  "Amazon ECS task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<ecs>*")))
    (apply 'make-comint-in-buffer "aws-ecs" buffer "~/.emacs.d/bin/aws_ecs" nil (list arg))))
(defun aws-s3 (arg)
  "Amazon S3 task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<s3>*")))
    (apply 'make-comint-in-buffer "aws-s3" buffer "~/.emacs.d/bin/aws_s3" nil (list arg))))
(defun aws-route53 (arg)
  "Amazon Route53 task with ARG."
  (interactive "stask name: ")
  (progn
    (setq buffer (get-buffer-create (concat "*aws<route53>*")))
    (apply 'make-comint-in-buffer "aws-route53" buffer "~/.emacs.d/bin/aws_route53" nil (list arg))))


;;; Heroku
(leaf heroku :ensure t)



(provide 'init-orchestration)
;;; init-orchestration.el ends here
