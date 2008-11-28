#|
Pattern matching on lists is provided by the list-match macro.  The
syntax (list-match expr clause ...) takes an input expr that evaluates
to a list.  Clauses are of the form (pattern expr [guard]),
consisting of a pattern that matches a list of a particular shape, an
optional fender that must succeed if the pattern is to match, and an
expr that is evaluated if the pattern matches.  There are four types
of patterns:

  - () -- Matches the null list.

  - (pat0 pat1 ...) -- Matches a list with length exactly equal to the
    number of pattern elements.

  - (pat0 pat1 ... . patRest) -- Matches a list with length at least
    as great as the number of pattern elements before the literal dot.
    PatRest is a list containing the remaining elements of the input
    list after the initial prefix of the list before the literal dot.

  - pat -- Matches an entire list.  Should always appear as the last
    clause; it's not an error to appear elsewhere, but subsequent
    clauses could never match.

Each pattern element may be:

  - An identifier -- Matches any list element.  Additionally, the
    value of the list element is bound to the variable named by the
    identifier, which is in scope in the fender and expr of the
    corresponding clause.  Each identifier in a single pattern must
    be unique.

  - A literal underscore -- Matches any list element, but creates no
    bindings.

  - A constant -- Matches if the expression equals the constant value,
    but creates no bindings.

  - A quote expression -- Matches if the expression equals the quote
    expression, but creates no bindings.

  - A quasiquote expression -- Matches if the expression equals the
    quasiquote expression, but creates no bindings.

All comparisons are made with equal?.  The patterns are tested in
order, left to right, until a matching pattern is found; if fender is
present, it must evaluate as non-#f for the match to be successful.
Pattern variables are bound in the corresponding fender and
expression.  Once the matching pattern is found, the corresponding
expr is evaluated and returned as the result of the match.  An error
is signaled if no pattern matches the input list.

A simple match expression that computes the length of a list is given
below; the first pattern is the null list, which forms the base of the
recursion, and the second pattern matches a non-null input list, using
an underscore to signal that the value of the list element is not
bound in the result expression:

    (define (len xs)
      (list-match xs
        (() 0)
        ((_ . xs) (+ 1 (len xs)))))

Fenders can test the common case where two list elements must be
identical; (unique eql? xs) casts out adjacent duplicates from an
input list:

    (define (unique eql? xs)
      (list-match xs
        (() '())
        ((x) (list x))
        ((x y . rest) (unique eql? (cons y rest)) (eql? x y))
        ((x . rest) (cons x (unique eql? rest)))))

A more complex example uses two nested matchers to merge two input
lists ordered by the lt? predicate:

    (define (list-merge lt? xx yy)
      (list-match xx
        (() yy)
        ((x . xs)
          (list-match yy
            (() xx)
            ((y . ys)
              (if (lt? y x)
                  (cons y (list-merge lt? xx ys))
                  (cons x (list-merge lt? xs yy))))))))

Pattern matching is performed by a macro that expands into a cond
expression with one clause per pattern; an auxiliary macro handles the
various types of pattern elements.  The complete implementation, which
is based on an idea of Jos Koot, is given below:
|#

;; changed the order of the guard with respect to the original

(library (list-match)
  (export let- list-match)
  (import (rnrs) (sweet-macros))

(def-syntax let-
  (syntax-match ()
    (sub (let- () lst body1 body2 ...)
         #'(if (null? lst) (let () body1 body2 ...)
               (error 'let- "Argument list not empty" lst)))
    (sub (let- name value body1 body2 ...)
         #'(let ((name value)) body1 body2 ...)
         (identifier? #'name))
    (sub (let- (arg1 arg2 ... . rest) lst body1 body2 ...)
         #'(let ((ls lst))
             (if (null? ls) (error 'let- "Arguments mismatch" 'lst)
                 (let- arg1 (car ls)
                       (let- (arg2 ... . rest) (cdr ls) body1 body2 ...))))
         (for-all identifier? #'(arg1 arg2 ...))
         (syntax-violation 'let- "Found non identifier in formals"
           #'(arg1 arg2 ...) (remp identifier? #'(arg1 arg2 ...))))
    ))

(def-syntax list-match-aux
  (syntax-match (quote quasiquote)
      (sub (_ obj pattern action)
           #'(list-match-aux obj pattern action #t))
      (sub (_ obj () action guard)
           #'(and (null? obj) guard action))
      (sub (_ obj underscore action guard)
           #'(and guard action)
           (and (identifier? #'underscore)(free-identifier=? #'underscore #'_)))
      (sub (_ obj var action guard)
           #'(let ((var obj)) (and guard action))
        (identifier? #'var))
      (sub (_ obj (quote datum) action guard)
           #'(and (equal? obj (quote datum)) guard action))
      (sub (_ obj (quasiquote datum) action guard)
           #'(and (equal? obj (quasiquote datum)) guard action))
      (sub (_ obj (kar . kdr) action guard)
           #'(and (pair? obj)
                  (let ((kar-obj (car obj)) (kdr-obj (cdr obj)))
                    (list-match-aux kar-obj kar
                      (list-match-aux kdr-obj kdr action guard)))))
      (sub (_ obj const action guard)
           #'(and (equal? obj const) guard action))
      ))

(def-syntax list-match
  (syntax-match (when)
    (sub (list-match lst (when pattern action guard ...) ...)
      #'(let ((ls lst))
          (cond
           ((list-match-aux ls pattern (list action) guard ...) => car) ...
           (else (error 'list-match "pattern mismatch" ls)))))))
)
