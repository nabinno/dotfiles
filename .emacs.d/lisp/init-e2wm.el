;;; init-e2wm.el --- e2wm configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'e2wm)
(require-package 'e2wm-sww)

(setq e2wm:debug nil)
(setq e2wm:c-max-history-num 20)
(setq e2wm:c-recordable-buffer-p
      (lambda (buf)
        (buffer-local-value 'buffer-file-name buf)))
(setq e2wm:c-document-buffer-p
      (lambda (buf)
        (string-match "\\*\\(Help\\|info\\|w3m\\|WoMan\\)"
                      (buffer-name buf))))
(setq e2wm:c-blank-buffer
      (let ((buf (get-buffer-create " *e2wm:blank*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (buffer-disable-undo buf)
          (erase-buffer)
          (setq buffer-read-only t)) buf))

;; keybinds
(setq e2wm:prefix-key "C-c w")
(e2wm:add-keymap
 e2wm:pst-minor-mode-keymap
 '(("<M-left>"  . e2wm:dp-code)                     ; change to code perspective
   ("<M-right>" . e2wm:dp-two)                      ; change to two perspective
   ("<M-up>"    . e2wm:dp-doc)                      ; change to doc perspectvie
   ("<M-down>"  . e2wm:dp-dashboard)                ; change to dashboard perspective
   ("C-."       . e2wm:pst-history-forward-command) ; go forward with history
   ("C-,"       . e2wm:pst-history-back-command)    ; go backward with history
   ("prefix L"  . ielm)
   ("M-m"       . e2wm:pst-window-select-main-command)
   )
 e2wm:prefix-key)


;;; Perspective customization - Code
;; layout
(setq e2wm:c-code-recipe ;; for 1440x900++ (default)
  '(| (:left-max-size 35)
      (- (:upper-size-ratio 0.7)
         files history)
      (- (:upper-size-ratio 0.7)
         (| (:right-max-size 30)
            main imenu)
         sub)))
;; (setq e2wm:c-code-recipe ;; for 1280x768
;;   '(| (:left-max-size 30)
;;       (- (:upper-size-ratio 0.7)
;;          files history)
;;       (- (:upper-size-ratio 0.7)
;;          (| (:right-max-size 25)
;;             main imenu)
;;          sub)))
;; (setq e2wm:c-code-recipe ;; for 1024x768
;;   '(| (:left-max-size 35)
;;       (- (:upper-size-ratio 0.7)
;;          (- (:upper-size-ratio 0.6)
;;             files imenu)
;;          history)
;;       (- (:upper-size-ratio 0.7)
;;          main sub)))
(setq e2wm:c-code-winfo
  '((:name main)
    (:name files :plugin files)
    (:name history :plugin history-list)
    (:name sub :buffer "*info*" :default-hide t)
    (:name imenu :plugin imenu :default-hide nil))
  )

;; main displayable (others to be sub displayable)
(setq e2wm:c-code-show-main-regexp
   "\\*\\(vc-diff\\)\\*")


;;; Perspective customization - Two
;; layout
(setq e2wm:c-two-recipe
      '(- (:upper-size-ratio 0.8)
          (| left
             (- (:upper-size-ratio 0.9)
                right history))
          sub))
(setq e2wm:c-two-winfo
      '((:name left )
        (:name right )
        (:name sub :buffer "*Help*" :default-hide t)
        (:name history :plugin history-list :default-hide nil)))
(setq e2wm:c-two-right-default 'left) ;; left: same buffer, prev: previous buffer

;; keybinds
(e2wm:add-keymap
 e2wm:dp-two-minor-mode-map
 '(("prefix I" . info)
   ("C->"      . e2wm:dp-two-right-history-forward-command) ; go forward with right-side history
   ("C-<"      . e2wm:dp-two-right-history-back-command)    ; go backward with right-side history
   )
 e2wm:prefix-key)


;;; Perspective customization - Doc
;; layout
(setq e2wm:c-doc-recipe
      '(- (:upper-size-ratio 0.75)
        (| left right)
        sub))
(setq e2wm:c-doc-winfo
      '((:name left)
        (:name right)
        (:name sub :default-hide t)))

;; keybinds
(e2wm:add-keymap
 e2wm:dp-doc-minor-mode-map
 '(("prefix I" . info))
 e2wm:prefix-key)


;;; Perspective customization - Dashboard
(setq e2wm:c-dashboard-plugins
  '(clock top
    (open :plugin-args (:command eshell :buffer "*eshell*"))
    (open :plugin-args (:command doctor :buffer "*doctor*"))
    ))


;;; E2WM Term
(require-package 'e2wm-term)


(provide 'init-e2wm)
;;; init-e2wm.el ends here
