;;; init-plantuml --- plantuml configuration
;;; Commentary:
;;; Code:
(require-package 'puml-mode)
(add-to-list 'auto-mode-alist '("\\.uml$" . puml-mode))

(setq puml-plantuml-jar-path "~/.local/bin/plantuml.jar")
(setq puml-java-options "")
(setq puml-java-options "-charset UTF-8")

(defun puml-java-execute ()
  "In puml-mode, execute PlanUML."
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML? "
                  'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
        out-file
        cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
               "java -jar " puml-java-options " "
               (shell-quote-argument puml-plantuml-jar-path) " "
               (and out-file (concat "-t" (file-name-extension out-file))) " "
               puml-java-options " "
               (buffer-file-name)))
    (message cmd)
    (shell-command cmd)
    (message "done")))

(defun puml-node-execute ()
  "Node-plantuml."
  (interactive)
  (progn
    (setq buffer (get-buffer-create (concat "*puml*")))
    (apply 'make-comint-in-buffer "node-plantuml" buffer
           "~/.emacs.d/bin/puml" nil (list (buffer-file-name)))
    ))

(setq puml-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-C") 'puml-java-execute)
        (define-key map (kbd "C-c C-c") 'puml-node-execute)
        map))


;; (require-package 'plantuml-mode)
;; (setq plantuml-jar-path "~/.local/bin/plantuml.jar")




(provide 'init-plantuml)
;;; init-plantuml.el ends here