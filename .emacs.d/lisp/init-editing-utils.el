;;; init-editing ---  editing configuration
;;; Commentary:
;;; Code:
(leaf unfill :ensure t)
(leaf whole-line-or-region :ensure t)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)


;;; Whitespace
(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                eww-mode
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))

(leaf whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode t))


;;; Newline behaviour
(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)


(when (eval-when-compile (string< "24.3.1" emacs-version))
  ;; https://github.com/purcell/emacs.d/issues/138
  (after-load 'subword
    (diminish 'subword-mode)))


;; (when (fboundp 'global-prettify-symbols-mode)
;;   (global-prettify-symbols-mode))


(leaf undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode)
  (global-set-key (kbd "M-/") 'undo-tree-undo)
  (setq undo-tree-auto-save-history nil))


(leaf highlight-symbol
  :ensure t
  :config
  (dolist (hook '(prog-mode-hook html-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
  (eval-after-load 'highlight-symbol
    '(diminish 'highlight-symbol-mode)))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(leaf expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "M-=") 'er/expand-region)
  ;; (global-set-key (kbd "M-[ 1 ; 5 k") 'er/expand-region)
  )

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-.")             'set-mark-command)
;; (global-set-key (kbd "M-[ 1 ; 5 n")     'set-mark-command)
(global-set-key (kbd "M-SPC")           'set-mark-command)
(global-set-key (kbd "C-x C-.")         'pop-global-mark)
;; (global-set-key (kbd "C-x M-[ 1 ; 5 n") 'pop-global-mark)

(leaf ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "") 'ace-jump-word-mode))

(leaf multiple-cursors
  :ensure t
  :config
  ;; multiple-cursors
  (global-set-key (kbd "M-,")             'mc/mark-previous-like-this)
  (global-set-key (kbd "C-<")             'mc/mark-previous-like-this)
  ;; (global-set-key (kbd "M-[ 1 ; 6 l")     'mc/mark-previous-like-this)
  (global-set-key (kbd "M-.")             'mc/mark-next-like-this)
  (global-set-key (kbd "C->")             'mc/mark-next-like-this)
  ;; (global-set-key (kbd "M-[ 1 ; 6 n")     'mc/mark-next-like-this)
  (global-set-key (kbd "C-+")             'mc/mark-next-like-this)
  (global-set-key (kbd "M-+")             'mc/mark-next-like-this)
  ;; (global-set-key (kbd "M-[ 1 ; 6 k")     'mc/mark-next-like-this)
  (global-set-key (kbd "C-c C-<")         'mc/mark-all-like-this)
  ;; (global-set-key (kbd "C-c M-[ 1 ; 6 l") 'mc/mark-all-like-this)
  ;; From active region to multiple cursors:
  (global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
  (global-set-key (kbd "C-c c c") 'mc/edit-lines)
  (global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)
  )

;; ;; Train myself to use M-f and M-b instead
;; (global-unset-key [M-left])
;; (global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; Visual regexp
;;----------------------------------------------------------------------------
(leaf visual-regexp
  :ensure t
  :config
  (define-key global-map (kbd "M-r") 'vr/replace)
  (define-key global-map (kbd "C-M-m") 'vr/mc-mark))

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(leaf page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (setq page-break-lines-char ?-))

;;----------------------------------------------------------------------------
;; Fill column indicator
;;----------------------------------------------------------------------------
(when (eval-when-compile (> emacs-major-version 23))
  (leaf fill-column-indicator :ensure t)
  (defun sanityinc/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  ;;(add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

  (defun sanityinc/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
               (null popup-instances))
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (sanityinc/fci-enabled-p)
          (turn-on-fci-mode))))))


;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(leaf move-dup
  :el-get wyuenho/move-dup
  :config
  (global-set-key [M-up]              'move-dup-move-lines-up)
  ;; (global-set-key (kbd "M-[ 1 ; 3 A") 'move-dup-move-lines-up)
  (global-set-key [M-down]            'move-dup-move-lines-down)
  ;; (global-set-key (kbd "M-[ 1 ; 3 B") 'move-dup-move-lines-down)
  (global-set-key [M-S-up]            'move-dup-move-lines-up)
  ;; (global-set-key (kbd "M-[ 1 ; 4 A") 'move-dup-move-lines-up)
  (global-set-key [M-S-down]          'move-dup-move-lines-down)
  ;; (global-set-key (kbd "M-[ 1 ; 4 B") 'move-dup-move-lines-down)
  (global-set-key (kbd "C-c p") 'move-dup-duplicate-down))

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(whole-line-or-region-global-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)


(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point-marker))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))


;;; Ag
(when (executable-find "ag")
  (leaf ag :ensure t)
  (leaf wgrep-ag :el-get mhayashi1120/Emacs-wgrep)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))


;; Avy
(leaf avy
  :ensure t
  :config
  ;; (global-set-key (kbd "M-[ 1 ; 5 n") 'avy-goto-char)
  ;; (global-set-key (kbd "M-[ 1 ; 5 l") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)
  (avy-setup-default))


;;; Folding
;; (leaf fold-dwim :ensure t)
;; (global-set-key (kbd "<f8>")     'fold-dwim-toggle)
;; (global-set-key (kbd "<M-f8>")   'fold-dwim-hide-all)
;; (global-set-key (kbd "<S-M-f8>") 'fold-dwim-show-all)

;; (leaf fold-this :ensure t)
;; (setq fold-this-persistent-folds t)
;; (defun fold-this--this-or-all (all)
;;   (interactive "P")
;;   (let ((x (bounds-of-thing-at-point 'sexp))
;;         (rap (region-active-p)))
;;     (funcall (if all 'fold-this-all 'fold-this)
;;              (if rap (region-beginning) (car x))
;;              (if rap (region-end) (cdr x))))
;;   (message (substitute-command-keys "To unfold all, try \\[fold-this-unfold-all]")))
;; (with-eval-after-load "view"
;;   (define-key view-mode-map (kbd "i") 'fold-this--this-or-all)
;;   (define-key view-mode-map (kbd ",") 'fold-this-unfold-at-point)
;;   (define-key view-mode-map (kbd "f") 'fold-this-unfold-all))


;;; Lice, Lorem ipsum
(leaf lice :ensure t)
(leaf lorem-ipsum :ensure t)


(leaf highlight-escape-sequences :ensure t)
(hes-mode)


(leaf guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n"))
  (guide-key-mode 1)
  (diminish 'guide-key-mode))


;; ;;; Time/Date
;; (leaf time-ext :ensure t)
;; (leaf wwtime :ensure t)


;;; Normalize
(defun ucs-normalize-NFC-buffer ()
  "Normalize character codes with NFC in this buffer."
  (interactive)
  (setq buffer-read-only nil)
  (ucs-normalize-NFC-region (point-min) (point-max)))

(defun ucs-normalize-NFC-buffer-in-read-only ()
  "Normalize character codes with NFC in this read-only buffer."
  (interactive)
  (setq buffer-read-only nil)
  (ucs-normalize-NFC-region (point-min) (point-max))
  (setq buffer-read-only t))

(defun recenter-top-bottom-with-normalize-NFC-buffer ()
  "Setup recenter top-bottom with normalize-NFC-buffer."
  (interactive)
  (recenter-top-bottom)
  (ucs-normalize-NFC-buffer))

(defun recenter-top-bottom-with-normalize-NFC-buffer-in-read-only ()
  "Setup recenter top-bottom with normalize-NFC-buffer-in-read-only."
  (interactive)
  (recenter-top-bottom)
  (ucs-normalize-NFC-buffer-in-read-only))

(global-set-key (kbd "C-l") 'recenter-top-bottom-with-normalize-NFC-buffer)
(global-set-key (kbd "C-M-l") 'recenter-top-bottom-with-normalize-NFC-buffer-in-read-only)



(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
