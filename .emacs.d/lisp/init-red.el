;;; init-red --- red configuration
;;; Commentary:
;;; Code:
(unless (require 'red nil 'noerror)
  (el-get-bundle Skrylar/red.el))
(require 'red)
(setq red-indentation-amount 4)


;; Override red-indent-line
(defun red-get-indentation-for-line ()
  "Figures out the proper indentation for the current line."
  (save-excursion
    (if (bobp)
        0
      (let ((indentation 0)
            (closers-and-openers "[\])}][ \t]*[\[({]$")
            (closers "[\])}]")
            (openers "[\[({]")
            (potatos "[^])}\t ]"))
        ;; dedent if closers are present, but only if the line
        ;; contains nothing except for closers
        (let* ((bol (progn (beginning-of-line) (point)))
               (eol (progn (end-of-line) (delete-horizontal-space) (point)))
               (open (how-many openers bol eol))
               (close (how-many closers bol eol))
               (close-and-open (how-many closers-and-openers bol eol))
               (vegetables (how-many potatos bol eol))
               (diff (- open close close-and-open)))
          (if (or (and (= 0 vegetables) (< diff 0))
                  (and (> close-and-open 0) (< diff 0)))
              (setq indentation (* diff red-indentation-amount))))
        ;; add previous line's indentation
        (while (let* ((bol (progn (previous-line) (beginning-of-line) (point)))
                      (eol (progn (end-of-line) (delete-horizontal-space) (point))))
                 (= bol eol)))
        (setq indentation (+ indentation (current-indentation)))
        ;; indent if openers are present
        (let* ((bol (progn (beginning-of-line) (point)))
               (eol (progn (end-of-line) (delete-horizontal-space) (point)))
               (open (how-many openers bol eol))
               (close (how-many closers bol eol))
               (close-and-open (how-many closers-and-openers bol eol))
               (diff (- (+ open close-and-open) close)))
          (if (> diff 0)
              (setq indentation (+ indentation (* diff red-indentation-amount)))))
        (max 0 indentation)))))

(defun red-indent-line ()
  "Indents the current line using Red's indentation rules."
  (interactive "*")
  (indent-line-to (red-get-indentation-for-line))
    (back-to-indentation))")]""})]"))))))


(provide 'init-red)
;;; init-red.el ends here
