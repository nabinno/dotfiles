;;; init-mu4e --- mu4e configuration
;;; Commentary:
;;; Code:
(require 'mu4e)
(require-package 'helm-mu)
(require-package 'mu4e-maildirs-extension)

(setq mu4e-maildir                  "~/Maildir"
      mu4e-attachment-dir           "~/Maildir/Downloads"
      mu4e-sent-folder              "/Work/INBOX.Sent"
      mu4e-drafts-folder            "/Work/INBOX.Drafts"
      mu4e-get-mail-command         "offlineimap"
      mu4e-user-mail-address-regexp "mywork-addres\\|mygmail-address"
      mu4e-html2text-command        "html2text -utf8 -width 72"
      mu4e-update-interval 300
      mu4e-use-fancy-chars nil
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      ;; mu4e-html2text-command nil
      mu4e-trash-folder (lambda (msg)
                          (cond
                           ((string-match "/Work/"  (or (mu4e-message-field msg :maildir) "")) "/Work/INBOX.Trash")
                           ((string-match "/Gmail/" (or (mu4e-message-field msg :maildir) "")) "/Gmail/[Gmail].Trash")
                           )))
(setq mail-user-agent 'mu4e-user-agent)
(setq message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it)
(set-language-environment "UTF-8")


;;; SMTPMail
(require 'smtpmail)
(setq my-mu4e-account-alist
      '(("Gmail"
         (mu4e-sent-messages-behavior delete)
         (mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
         (mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
         (user-mail-address  "mygmail-address")
         (smtpmail-starttls-credentials  '(("smtp.gmail.com" 587 nil nil)))
         (smtpmail-auth-credentials      '(("smtp.gmail.com" 587 "mygmail-address" nil)))
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user           "mygmail-address")
         (smtpmail-smtp-server         "smtp.gmail.com")
         (smtpmail-smtp-service 587)
         (starttls-use-gnutls t)
         (message-signature nil)
         ;; add other variables here
         )))


;; ;;; Shortcut Keys
;; (add-to-list 'jk-mu4e-account-alist
;;              '("Work"
;;                (mu4e-sent-folder   "/Work/INBOX.Sent")
;;                (mu4e-drafts-folder "/Work/INBOX.Drafts")
;;                (smtpmail-smtp-user "work-username")
;;                (user-mail-address  "work-address")
;;                (mu4e-sent-messages-behavior sent)
;;                (starttls-use-gnutls nil)
;;                (smtpmail-default-smtp-server "work-smtpserver")
;;                (smtpmail-smtp-server         "worksmtpserver")
;;                (smtpmail-smtp-service secret)
;;                (message-signature (shell-command-to-string "cat ~/Mail/signature.txt"))
;;                ;; add other variables here
;;                ))
;; (defun jk-mu4e-set-account ()
;;   "Set the account for composing a message."
;;   (let* ((account
;;           (if mu4e-compose-parent-message
;;     (let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir)))
;;                 (string-match "/\\(.*?\\)/" maildir)
;;                 (match-string 1 maildir))
;;             (completing-read (format "Compose with account: (%s) "
;;                                      (mapconcat #'(lambda (var) (car var)) jk-mu4e-account-alist "/"))
;;                              (mapcar #'(lambda (var) (car var)) jk-mu4e-account-alist)
;;                              nil t nil nil (caar jk-mu4e-account-alist))))
;;          (account-vars (cdr (assoc account jk-mu4e-account-alist))))
;;     (if account-vars
;;     (mapc #'(lambda (var)
;;   (set (car var) (cadr var)))
;;               account-vars))))
;; (add-hook 'mu4e-compose-pre-hook 'jk-mu4e-set-account)
;; (setq mu4e-maildir-shortcuts
;;       '(("/Work/INBOX"              . ?i)
;;         ("/Work/INBOX.Sent"         . ?s)
;;         ("/Gmail/Gmail"             . ?g)
;;         ("/Gmail/INBOX"             . ?n)
;;         ("/Gmail/[Gmail].Sent Mail" . ?e)
;;         ("/Gmail/[Gmail].All Mail"  . ?a)
;;         ))


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
