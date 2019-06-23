;;; init-org --- org configuration
;;; Commentary:
;;; Code:
(when (< emacs-major-version 24)
  (require-package 'org))
;; (require-package 'org-fstree) ;;; TODO
(when *is-a-mac*
  (require-package 'org-mac-link)
  (autoload 'org-mac-grab-link "org-mac-link" nil t)
  (require-package 'org-mac-iCal))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)

; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)"))))


;;; Org capture
(setq org-capture-templates
      '(
        ("m" "Memo" entry (file+headline nil "Memos") "** %?\n   %T")
        ("M" "Memo (with file link)" entry (file+headline nil "Memos") "** %?\n   %a\n   %T")))

;; code reading
(defvar org-code-reading-software-name nil)
(defvar org-code-reading-file "code-reading.org")
(defun org-code-reading-read-software-name ()
  (set (make-local-variable 'org-code-reading-software-name)
       (read-string "Code Reading Software: "
                    (or org-code-reading-software-name
                        (file-name-nondirectory
                         (buffer-file-name))))))
(defun org-code-reading-get-prefix (lang)
  (let ((sname (org-code-reading-read-software-name))
        (fname (file-name-nondirectory
                (buffer-file-name))))
    (concat "[" lang "]"
            "[" sname "]"
            (if (not (string= sname fname)) (concat "[" fname "]")))))
(defun org-capture-code-reading ()
  (interactive)
  (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (org-capture-templates
          '(("c" "Code Reading" entry (file+headline (concat org-directory org-code-reading-file) "Code Readings") "** %(identity prefix) %?\n   %a\n   %T")
            )))
    (org-capture nil "c")))
(define-key global-map "\C-xz" 'org-capture-code-reading)


;; ;;; Org babel
;; ;; plantuml
;; (setq org-plantuml-jar-path "~/.local/bin/plantuml.jar")
;; (defun org-mode-init ()
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    (add-to-list 'org-babel-load-languages '(plantuml . t))))
;; (add-hook 'org-mode-hook 'org-mode-init)


;;; OX
(require-package 'ox-gfm)

(unless (require 'ox-qmd nil 'noerror)
  (el-get-bundle 0x60df/ox-qmd))


;;; Org clock
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

(require-package 'org-pomodoro)
(after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))

;; ;; Show iCal calendars in the org agenda
;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
;;   (setq org-agenda-include-diary t
;;         org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0))))))

(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil))
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;; (after-load 'org
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((R . t)
;;      (ditaa . t)
;;      (dot . t)
;;      (emacs-lisp . t)
;;      (gnuplot . t)
;;      (haskell . nil)
;;      (latex . t)
;;      (ledger . t)
;;      (ocaml . nil)
;;      (octave . t)
;;      (python . t)
;;      (ruby . t)
;;      (screen . nil)
;;      (sh . t)
;;      (sql . nil)
;;      (sqlite . t))))


;;; org-trello
(require-package 'org-trello)


;;; Blog engine
;; org-page
(require-package 'org-page)
(require 'org-page)
(setq op/repository-directory "~/nabinno.github.io")
(setq op/site-domain "https://nabinno.github.io/")

;; blog-admin
(require-package 'blog-admin)
(require 'blog-admin)
;; (setq blog-admin-backend-type 'org-page)
(setq blog-admin-backend-path "~/nabinno.github.io/org")
(setq blog-admin-backend-new-post-in-drafts t)
(setq blog-admin-backend-new-post-with-same-name-dir t)
(setq blog-admin-backend-org-page-drafts nil)
(setq blog-admin-backend-org-page-config-file "~/nabinno.github.io/config.el")

;; ox-publish
(require 'ox-publish)
(setq org-publish-project-alist
      '(("org-ghp-posts"
         ;; path to your org files.
         :base-directory "~/nabinno.github.io/org/"
         :base-extension "org"
         ;; path to your jekyll files.
         :publishing-directory "~/nabinno.github.io/jekyll/"
         :recursive t
         :publishing-function org-md-publish-to-md
         :headline-levels 4
         :markdown-extension "md"
         ;; only export section between <body> </body>
         :body-only t)
        ("org-ghp-assets"
         :base-directory "~/nabinno.github.io/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/nabinno.github.io/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("org-ghp"
         :components ("org-ghp-posts" "org-ghp-assets"))))


;;; org-tree-slide
(require-package 'org-tree-slide)


;;; Temporary setting
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c B") 'googlecl-prompt-blog)
(global-set-key (kbd "C-c L") 'org-googlecl-list-blogs)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(setq org-hide-leading-stars t)
(setq org-directory "~/")
(setq org-default-notes-file "notes.org")
(setq org-tag-alist
      '(
        ("Heroku" . ?k)
        ("Rails" . ?r)
        ))

;; html
(setq org-export-html-validation-link nil)
(setq org--html-postamble nil)
(setq org-html-validation-link nil)

;; agenda
(setq org-agenda-files (list org-directory))
(setq calendar-holidays nil)

;; key bind
(add-hook 'org-mode-hook
          (lambda ()
            (mapc (lambda (pair)
                    (let ((key (car pair))
                          (func (cdr pair)))
                      (define-key org-mode-map
                        (read-kbd-macro key) func)))
                  '(("C-<tab>" . other-window)
                    ;; ("M-[ 1 ; 5 i" . other-window)
                    ("S-M-j" . org-insert-todo-heading)
                    ("M-J" . org-insert-todo-heading)
                    ("?" . org-insert-subheading)
                    ("?" . org-backward-heading-same-level)
                    ("?" . org-forward-heading-same-level)
                    ("C-M-p" . org-move-subtree-up)
                    ("C-M-n" . org-move-subtree-down)
                    ("<prior>" . org-shiftup)
                    ("M-o h" . org-shiftleft)
                    ("M-o f" . org-shiftright)
                    ("<next>" . org-shiftdown)
                    ("ESC <prior>" . org-shiftmetaup)
                    ;; ("ESC M-[ 5 ~" . org-shiftmetaup)
                    ("ESC M-O h" . org-shiftmetaleft)
                    ("ESC M-O f" . org-shiftmetaright)
                    ("ESC <next>" . org-shiftmetadown)
                    ;; ("ESC M-[ 6 ~" . org-shiftmetadown)
                    ;; ("" . org-shiftcontrolup)
                    ;; ("" . org-shiftcontrolleft)
                    ;; ("" . org-shiftcontrolright)
                    ;; ("" . org-shiftcontroldown)
                    ))))



(provide 'init-org)
;;; init-org.el ends here
