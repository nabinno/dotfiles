;;; init-red --- red configuration
;;; Commentary:
;;; Code:
(unless (require 'red nil 'noerror)
  (el-get-bundle Skrylar/red.el))
(require 'red)
(setq red-indentation-amount 4)


;; Override red-words-regex
(setq red-words-regex
      (concat "\\<"
              (regexp-opt-group
               (split-string
                "
NaN? a-an about absolute acos action? add ail alias all and and~ any any-block? any-function? any-object? any-path?
any-string? any-word?  append arccosine arcsine arctangent arctangent2 as as-pair asin ask at atan atan2 attempt back
bind bitset? block? body-of break byte!  c-string! case catch cause-error cdecl char? charset clear comment complement
complement? complete-from-path compose cond construct context continue copy cos cosine datatype? declare
default-input-completer dehex difference divide do does e either empty? equal? error? eval-set-path even? exclude exit
exp extend false fifth file? find first float! float32! float? forall foreach forever form fourth func function
function? get get-path? get-word?  greater-or-equal? greater? halt has hash? head head? help if in index?  input insert
integer! integer? intersect issue? keys-of last last-lf?  length? lesser-or-equal? lesser? lit-path? lit-word? load
local log-10 log-2 log-e logic! logic? loop lowercase make map? max min modify modulo mold multiply native? negate
negative? next none?  not not-equal? object object? odd? op? or or~ pad pair? paren? parse parse-trace path? percent?
pick pointer! poke positive? power prin print probe put quit quit-return quote random read-input reduce refinement?
reflect remainder remove repeat replace return reverse round routine? same? second select series? set set-buffer-history
set-path? set-word? shift shift-left shift-logical shift-right sin sine skip sort source spec-of square-root stats
strict-equal? string?  struct! subtract swap switch tail tail? take tan tangent third throw to to-hex trim true try
tuple? type? typeset? union unique unless unset? axis until uppercase url? value? values-of variadic vector?  what while
word? words-of xor xor~ zero? ctx fn |>")
               t t)
              "\\>"))
(defconst red-constant-regex
  "\\<\\([A-Z]\\(\\w\\|\\-\\)*\\)\\>"
  "Definition of Red constant.")
(defconst red-red-regex
  "^\\<Red\\> ")
(defconst red-font-lock-keywords
  `((,red-red-regex . font-lock-warning-face)
    (,red-constant-regex . font-lock-string-face)
    (,red-tuple-regex . font-lock-constant-face)
    (,red-percent-regex . font-lock-constant-face)
    (,red-float-regex . font-lock-constant-face)
    (,red-hexadecimal-regex . font-lock-constant-face)
    (,red-integer-regex . font-lock-constant-face)
    (,red-words-regex . font-lock-function-name-face)
    (,red-setter-regex . font-lock-variable-name-face))
  "Font lock table for the Red programming language")


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
               (diff (if (= open close-and-open)
                         (- (+ open close-and-open) close)
                       (- open close))))
          (if (> diff 0)
              (setq indentation (+ indentation (* diff red-indentation-amount)))))
        (max 0 indentation)))))

(defun red-indent-line ()
  "Indent the current line using Red's indentation rules."
  (interactive "*")
  (indent-line-to (red-get-indentation-for-line))
  (back-to-indentation))


(provide 'init-red)
;;; init-red.el ends here
