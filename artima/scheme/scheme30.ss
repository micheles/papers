#|Runtime pattern matching and object oriented APIs
=======================================================

As a final example of macros power, this episode will implement a syntax
for full runtime pattern matching of lists .

.. _13: http://www.artima.com/weblogs/viewpost.jsp?thread=248953
.. _15: http://www.artima.com/weblogs/viewpost.jsp?thread=249681
.. _association lists:

list-match
----------------------------------------------------------------

In episode 15_ I have discussed a very limited form of pattern
matching, i.e. list destructuring. There is more to
pattern matching than that.

A general list matcher should be able to
destructure a list, whatever complicate it may be, and to perform
*different* actions for different structures. In this sense, pattern
matching is a kind of if statement on steroids.

The Scheme standard does not provide a default list matcher, but there
are tons of libraries providing such a feature, and in the appendix
we will provide an implementation of ``list-match`` ourselves, so
let us assume we have list matcher accepting the following patterns:

.. code-block:: scheme

 > (list-match <patterns>)
 ((list-match lst (sub pattern action guard ...) ...))

Given an object (which usually is a list), the matcher compares
it with various patterns, and perform different actions depending
on the match, which can be further restricted with a guard.
Here are a few trivial tests:

$$MATCH-TESTS

As usual, the understore ``_`` means "match but do not bind.
Here are two further tests, that makes use of the following helper function:

$$MATCH-ODD

``replace-odd`` makes use of guarded patterns to replace the odd
numbers in a list of numbers with the string "odd":

$$MATCH-ODD1

``replace-odd`` only recognizes numbers and lists: it chokes on strings
or any other kind of object, by raising a ``pattern failure`` error:

$$MATCH-ODD2

This example is simple, but it does not make justice to the power of
pattern matching. The following example, however, will do (or at least
I hope so): it explains how to implement object oriented APIs in terms
of pattern matching functions.

Pattern matching vs message passing
-------------------------------------------------------

A typical use case for
full pattern matching is message passing: in functional languages you
can send lists as messages to an object, and the object will respond
differently depending on the structure of the list.  This is the
standard mechanism to send messages in Erlang, but can be done in any
functional language with pattern matching.

I you think about it for a minute, you will recognize that pattern
matching makes the creation of object oriented APIs rather trivial.
Let consider for instance Scheme `association lists`_, which by default
have a functional API, and suppose we want to provide an object oriented
interface on top on it, say a Python-like API. The task can be easily
performed via pattern matching, as follows:

$$ALIST

Here we used the R6RS procedures ``(assq k a)`` to extract the slot
corresponding to the key ``k`` from the association list ``a``, as
well as the procedure ``set-cdr!`` described in episode 13_ to modify
the ``cdr`` of a slot (imported from ``(rnrs mutable-pairs)``).
Notice that I am using the term *slot* here to refer to
the inner lists that compose an association list.

Here are a few tests:

$$ALIST-TESTS

Appendix: implementation of ``list-match``
-------------------------------------------------------------

The implementation below is not my own, I have adapted
(i.e. shamelessly copied) it from Phil Bewig, which in turns copied it
from Jos Koot, which maybe is the original author, or maybe he copied
it from some other source.  But attribution is not important, the
important thing is the code:

$$aps/list-match:

The implementation above makes use of a few tricks which are well known
to experienced Schemers, but may look unfamiliar to my readers.

First of all, we leverage on the fact that ``(and boolean expr)`` returns
``expr`` if ``boolean`` is true and ``#f`` otherwise, i.e. it is equivalent
to ``(if boolean expr #f)``. Then, we use the ``=>`` form of the conditional
expression.
As explained in the `cond specification`, a ``cond`` clause may
have the form ``(value => func)``; when the value is not false,
then the function is applied to it and ``cond`` returns the result,
otherwise ``cond`` looks at the next clause. For instance

.. code-block:: scheme

 > (cond (#f => any-function) ('(1) => car))
 1

Notice that the underscore is a special identifier in macros
(just as the ellipsis identifier ``...``` is special) and it
cannot be used as a literal, this is why the trick
``(free-identifier=? #'_ underscore)`` is used.

I have exported the internal macro ``_match``, to make it
possible to understand how the implementation works. ``_match`` is
able to match a single pattern. For instance we have

.. code-block:: scheme

 > (_match '(1 2) (x y) (list (+ x y)))
 (3)

Using ``syntax-expand`` we can see the inner working of ``_match``:

.. code-block:: scheme

 > (syntax-expand(_match '(1 2) (x y) (list (+ x y)) #t))
 (let ((ob '(1 2)))
  (and (pair? ob)
       (let ((kar-obj ob) (kdr-obj (cdr ob)))
         (_match kar-obj x
           (_match kdr-obj (y) (list (+ x y)) #t)))))


.. _cond specification: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_376

|#

(import (rnrs) (sweet-macros) (aps list-utils) (aps list-match)
        (rnrs mutable-pairs) (aps easy-test) (aps compat))
                    
;;ALIST
;; a function performing pattern matching which provides a Pythonic
;; dictionary interface over an association list
(define (alist . a)
  (lambda args
    (list-match args
     (sub () a); return the underlying alist
     (sub ('->keys); return the keys 
          (map car a))
     (sub ('->values); return the values
          (map cdr a))
     (sub (k); emulate Python .__getitem__
          (let ((slot (assq k a)))
            (if slot
                (cdr slot)
                (error 'alist (format "Missing key ~a" k)))))
     (sub (k 'or val); emulate Python .get
          (let ((slot (assq k a)))
            (if slot
                (cdr slot)
                val)))
     (sub (k '! val); emulate Python .__setitem__
          (let ((slot (assq k a)))
            (if slot
                ;; modify existing slot
                (begin (set-cdr! slot val) a)
                ;; else append new slot    
                (let ((new-a (append a (list (cons k val)))))
                  (set! a new-a) a))))
     )))
;;END

(define a (alist '(x . 1) '(y . 2)))

;;MATCH-ODD
(define (replace-odd obj)
  (list-match obj
    (sub x "odd" (and (integer? x) (odd? x)))
    (sub x x (integer? x))
    (sub x (map replace-odd x) (list? x))
    ))
;;END

(run

 (test "patterns"
       (list-match <patterns>)
       '((list-match lst (sub pattern template guard ...) ...)))

 ;;MATCH-TESTS
 (test "match1"
       (list-match '(1) (sub (x) x))
       1)
 
 (test "match2"
       (list-match '(1 2) (sub (_ y) y))
       2)

 (test "match3"
       (list-match '(1 (2 3)) (sub (_ (_ z)) z))
       3)
 ;;END

 ;;MATCH-ODD1
 (test "match-odd1"
       (replace-odd '(1 (2 3)))
       '("odd" (2 "odd")))
 ;;END

 ;;MATCH-ODD2
 (test "match-odd2"
       (catch-error (replace-odd "string"))
       "pattern failure")
 ;;END
 
 ;;ALIST-TESTS
 (test "alist1"
       (a 'x)
       1)
 
 (test "alist2"
       (a 'y)
       2)

 (test "alist3"
       (catch-error (a 'z))
       "Missing key z")
 
 (test "alist4"
       (a 'z 'or 3)
       3)
 
 (test "alist5"
       (a 'z '! 3)
       '((x . 1) (y . 2) (z . 3)))
 
 (test "alist6"
       (a 'y '! 4)
       '((x . 1) (y . 4) (z . 3)))

 (test "alist7"
       (a '->keys)
       '(x y z))

 (test "alist8"
       (a '->values)
       '(1 4 3))
 ;;END
 )

; (test "match4"
;       (list-match '(1 (2 (x 4) 5) 6)
;                   (sub x (and (integer? x) (odd? x)) "odd")
;                   (sub x (and (integer? x) (odd? x)) "odd")) 1)
