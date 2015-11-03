;;; init-mu4e --- mu4e configuration
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/site-lisp/mu4e")
(require 'mu4e)
(require-package 'helm-mu)
(require-package 'mu4e-maildirs-extension)

(setq mu4e-maildir                  "~/Maildir"
      mu4e-attachment-dir           "~/Maildir/Downloads"
      mu4e-get-mail-command         "offlineimap"
      mu4e-user-mail-address-regexp "my-email-address"
      mu4e-html2text-command        "html2text -utf8 -width 72"
      mu4e-update-interval 300
      mu4e-use-fancy-chars nil
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      mu4e-trash-folder (lambda (msg)
                          (cond
                           ((string-match "/Gmail/" (or (mu4e-message-field msg :maildir) "")) "/Gmail/[Gmail].Trash")))
      mu4e-maildir-shortcuts '(("/Gmail/Gmail"               . ?g)
                               ("/Gmail/INBOX"               . ?i)
                               ("/Gmail/[Gmail].Sent Mail"   . ?s)
                               ("/Gmail/[Gmail].All Mail"    . ?a)
                               ("/Work/Gmail"              . ?G)
                               ("/Work/INBOX"              . ?I)
                               ("/Work/[Work].Sent Mail" . ?S)
                               ("/Work/[Work].All Mail"  . ?A)))
(setq mail-user-agent 'mu4e-user-agent)
(setq message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it)
(set-language-environment "UTF-8")


;;; SMTPMail
(require 'smtpmail)
(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))
;; ask for account when composing mail
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
(setq my-mu4e-account-alist
      '(("Gmail"
         (mu4e-sent-messages-behavior delete)
         (mu4e-sent-folder   "Gmail/[Gmail].Sent Mail")
         (mu4e-drafts-folder "Gmail/[Gmail].Drafts")
         (user-mail-address  "my-email-address")
         (user-full-name  "Foo Manchu")
         (smtpmail-starttls-credentials  '(("smtp.gmail.com" 587 nil nil)))
         (smtpmail-auth-credentials      '(("smtp.gmail.com" 587 "my-email-address" nil)))
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user           "my-email-address")
         (smtpmail-smtp-server         "smtp.gmail.com")
         (smtpmail-smtp-service 587)
         (starttls-use-gnutls t)
         (message-signature nil)
         ;; add other variables here
         )
        ("Work"
         (mu4e-sent-messages-behavior delete)
         (mu4e-sent-folder   "Work/[Work].Sent Mail")
         (mu4e-drafts-folder "Work/[Work].Drafts")
         (user-mail-address  "work-email-address")
         (user-full-name  "Foo Manchu")
         (smtpmail-starttls-credentials  '(("smtp.gmail.com" 587 nil nil)))
         (smtpmail-auth-credentials      '(("smtp.gmail.com" 587 "work-email-address" nil)))
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user           "work-email-address")
         (smtpmail-smtp-server         "smtp.gmail.com")
         (smtpmail-smtp-service 587)
         (starttls-use-gnutls t)
         (message-signature nil)
         ;; add other variables here
         )))


;;; Org-MU4E
(require 'org-mu4e)
(setq mu4e-headers-fields
      '((:date    . 25)
        (:flags   . 6)
        (:from    . 22)
        (:subject . nil)))


;; Encrypting Password
(require 'epa-file)
(epa-file-enable)
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(require 'netrc)
(defun offlineimap-get-password (host port)
  (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
         (hostentry (netrc-machine netrc host port port)))
    (when hostentry (netrc-get hostentry "password"))))


(provide 'init-mu4e)
;;; init-mu4e.el ends here.
