;;; init-red --- red configuration -*- lexical-binding: t -*-
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
NaN? action? any-block? any-function? any-object? any-path?
any-string? any-word? bitset? block? char? complement? datatype?
empty? equal? error? float! even? file? float32! float? function?
get-path? get-word? greater-or-equal? greater? integer! hash?
head? index? integer? issue? last-lf? length? lesser-or-equal?
lesser? lit-path? lit-word? logic? map? native? negative? none?
not-equal? object? odd? op? pair? paren? path? percent? pointer!
positive? refinement? routine? same? series? set-path? set-word?
strict-equal? string? struct! tail? tuple? type? typeset? unset?
url? value? vector? word? zero? byte! c-string! logic!  |> a-an
about all and and~ any as as-pair asin xor xor~ atan atan2 case
catch cause-error clear ctx cond construct context continue does
unless until not object or or~ if try to func function fn forall
foreach forever extend either exclude false true union declare
quote what while loop local help repeat source quit-return return
skip quit next do load break switch wait
layout left right panel white offset area rate base space text-list
collect
")
               t t)
              "\\>"))
(setq red-words-2-regex
      (concat "\\<"
              (regexp-opt-group
               (split-string
                "
arccosine arcsine arctangent arctangent2 absolute acos add ail
alias append ask at cos cosine divide attempt axis back
bind body-of cdecl charset comment complement complete-from-path
compose copy default-input-completer dehex difference e
eval-set-path exit exp fifth find first form fourth halt has
head in input insert intersect keys-of last log-10 log-2 log-e
lowercase make max min modify modulo mold multiply negate pad
parse parse-trace pick poke power prin print probe random
read-input reduce reflect remainder remove replace reverse round
second select set set-buffer-history shift shift-left
shift-logical shift-right sin sine sort spec-of square-root stats
subtract swap tail take tan tangent third throw
trim unique uppercase values-of variadic words-of
text button title on-create on-up on-down style on-over on-time group-box
scroller show make-fonts draw view unview do-actor do-events to-image set-focus size-text
write read
"
                )
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
    (,red-words-regex . font-lock-keyword-face)
    (,red-words-2-regex . font-lock-function-name-face)
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
