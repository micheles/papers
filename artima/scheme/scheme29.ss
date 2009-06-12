#|Comparing identifiers
==============================================================

What does it mean that two identifiers are equal in a lexically
scoped language with hygienic macros?

Take for instance this piece of code:

.. code-block:: scheme

 (let ((a 1))
     (let ((a 2))
        ...))

Is the first ``a`` equal to the second ``a``? They are bound to
different values, and they may be considered equal or not.
Moreover, consider the following:

.. code-block:: scheme

 (let ((a 1))
     (let ((b a))
        ...))

Here ``a`` and ``b`` are different names for the same value. Should they
be considered equal?

Finallly, consider what happens in a macro introducing dummy
identifiers:

.. code-block:: scheme

 (def-syntax (macro x)
   #'(let ((dummy 1))
       ...))

What happens if I pass to the macro an ``x`` argument named ``dummy``?
Should it be considered equal to the identifier introduced by the ``let``
form or not?


As a matter
of fact the Scheme standard define two different equality procedures
for identifiers (``bound-identifier=?`` and ``free-identifier=?``); to
those, I will add a third one, ``raw-identifier=?``:

$$lang:RAW-IDENTIFIER=?

This episode will be spent discussing the subtilities of identifier
equality.

Using ``raw-identifier=?``
-----------------------------------------------

The simplest procedure by far is ``raw-identifier=?``: two
identifiers are ``raw-identifier=?`` if they are equal as symbols.
For convenience, I have added the
``raw-identifier=?`` precedure in the ``(aps lang)`` library.
``raw-identifier=?`` can be used to manage duplicate names in
macros defining multiple identifiers at once, or in macros
defining tables names->values, such as the ``static-map``
we discussed in episode 22.

$$STATIC-MAP

Using ``bound-identifier=?``
-----------------------------------------------

``raw-identifier=?`` is simple and easy to understand, but it cannot
be used in all situations. Consider for instance the very first macro
we wrote, in episode 9_:

.. code-block:: scheme

  (def-syntax (multi-define (name1 name2 ...) (value1 value2 ...))
    #'(begin (define name1 value1) (define name2 value2) ...))

It is quite common to write macros defining multiple bindings, such
as ``multi-define``. There is also the common situation of
macros which expand themselves to macros defining
multiple bindings. When code is generated automatically, errors are
more than likely and it makes sense to manage explicitly
the situation when the list of names to be defined contains some
duplicate. ``multi-define`` as written does not perform any check, so that it
relies on the standard behavior of R6RS scheme, raising an error.
Unfortunately, the standard behavior only applies to programs and scripts,
whereas the REPL is quite free to behaves differently and indeed it does:

.. code-block:: scheme

 > (multi-define (a a) (1 2))
 a
 2

(in the REPL latter definitions override previous definitions). Being
unhappy with that, we can introduce a ``bound-identifier=?`` check
and raise a custom exception:

.. code-block:: scheme

 > (multi-define (a a) (1 2))
 Unhandled exception
  Condition components:
    1. &who: multi-define
    2. &message: "Found duplicated identifier in"
    3. &syntax:
        form: (a a)
        subform: #f

This is a case where using ``raw-identifier=?`` would not work.
Here is a (made-up) example explaining the problem with ``raw-identifier=?``
in the presence of dummy identifiers introduced by macros. Consider
the following macro expanding to ``multi-define``:

$$MULTI-DEFINE2

The macro introduces a dummy identifier ``id2``. What happens if
we call ``multi-define2`` with argument ``id`` equal to  ``id2``?

 > (multi-define2 id2 1)
 id2
 1

The answer is nothing, since we defined ``multi-define`` in terms of
``bound-identifier=?``, and two identifiers are equal according to
``bound-identifier=?`` only if they have the same name *and* the same
marks. In this case the identifier ``id2`` introduced by the macro
has different marks from the identifier ``id2`` used as an argument
in the macro. Had we defined ``multi-define`` in
terms of ``raw-identifier=?``, we would have had a spurious name
clash.

Using ``free-identifier=?``
------------------------------------

``free-identifier=?`` is the trickiest equality predicate. It is able
to determinate if two bound identifiers are the same apart possibly for
a rename at import time:

.. code-block:: scheme

 > (import (only (aps list-utils) range))
 > (import (rename (aps list-utils) (range r)))
 > (free-identifier=? #'r #'range)
 #t

Both ``raw-identifier=?`` and ``bound-identifier=?`` would fail to
recognize the identity of ``range`` and ``r`` in this case.
Notice that two aliases (same name, same binding) are *not*
considered ``free-identifier=?``:

.. code-block:: scheme

 > (define a 1)
 > (define b a) ;; alias for a 
 > (free-identifier=? #'a #'b)
 #f

Moreover

For things like cond which need to distinguish macro-specific literals
from bound names (e.g.: (let ((else 1)) (cond (else ---)))),
free-identifier=? is the right predicate.

For your convenience I have defined a ``compare-ids`` macro that can be
used to determine how two identifiers compare with respect to the different
equality operators:

$$COMPARE-IDS

(notice the usage of ``syntax->datum``: a list of literals
is generated, quoted and then converted into a syntax
object in the context of the macro, so that
``(compare-ids id1 id2)`` expands into a list of literals).

Auxiliary keywords
----------------------------------

The R6RS document defines a set of special macros, such as ``_``, ``...``,
``else`` and ``=>``, which lives in the global namespace and are
available to all R6RS programs. Such macros are used as auxiliary
syntax in various special forms, like ``cond`` and ``syntax-case``;
for this reason they are usually called auxiliary keywords.
The existence of such global variables makes it impossible to redefine
at top-level in scripts (but it can be done at the REPL);
however they can be redefined locally, thus breaking
the macros using the auxiliary syntax. 

.. code-block:: scheme

 (import (rnrs))
 (display
  (let ((else #f))
    (cond (else 2))))

|#

(import (rnrs) (sweet-macros) (aps list-utils) (aps lang) (aps easy-test))

;;ALIST2
 (def-syntax (alist2 arg ...)
   (: with-syntax ((name value) ...) (normalize #'(arg ...))
     (if (for-all identifier? #'(name ...))
         #'(let* ((name value) ...)
             (list (list 'name name) ...))
         (syntax-violation 'alist "Found non identifier" #'(name ...)
                           (remp identifier? #'(name ...))))))
;;END

;;MULTI-DEFINE
(def-syntax (multi-define (name1 name2 ...) (value1 value2 ...))
    #'(begin (define name1 value1) (define name2 value2) ...)
    (distinct? bound-identifier=? #'(name1 name2 ...))
    (syntax-violation 'multi-define "Found duplicated identifier in"
                      #'(name1 name2 ...)))
;;END

;;MULTI-DEFINE2
(def-syntax (multi-define2 id value)
   #'(multi-define (id id2) (value 'dummy)))
;;END

;;STATIC-MAP
(def-syntax (static-map (name value) ...)
  #'(syntax-match (<names> name ...)
      (sub (ctx <names>) #''(name ...))
      (sub (ctx name) #'value)
      ...)
  (distinct? raw-identifier=? #'(name ...))
  (syntax-violation 'static-map "Found duplicated identifier in"
                    #'(name ...)))
;;END

;;COMPARE-IDS
(def-syntax (compare-ids id1 id2)
  (let ((res '()))
   (when (raw-identifier=? #'id1 #'id2)
     (set! res (cons 'raw= res)))
   (when (bound-identifier=? #'id1 #'id2)
     (set! res (cons 'bound= res))) 
   (when (free-identifier=? #'id1 #'id2)
     (set! res (cons 'free= res)))
   (datum->syntax #'compare-ids `',res)))
;; END

(def-syntax (compare id1 a id2 b)
  #'(let ((id1 a))
      (let ((id2 b))
        (compare-ids id1 id2))))

(multi-define (a b) (1 2))

;(import (rnrs) (sweet-macros) (aps list-utils))

(def-syntax (free-bound= id1 id2)
  #`(list #,(free-identifier=? #'id1 #'id2)
          #,(bound-identifier=? #'id1 #'id2)))

(display
 (let ([fred 17])
  (def-syntax (compare-with-fred id)
    #'(free-bound= fred id))
  (compare-with-fred fred))) ;=> (#t #f)

;;EXPAND-X
(def-syntax expand-x
  (syntax-match ()
    (sub (expand-x id) "bound x" (bound-identifier=? #'id #'x))
    (sub (expand-x id) "free x" (free-identifier=? #'id #'x))   
    (sub (expand-x id) "other")))
;;END

(run
 (test "free identifier"
       (expand-x x)
       "free x")
 (test "non-free identifier"
       (let ((x 1)) (display (expand-x x)))
       "other")
 (test "other identifier"
       (expand-x y)
       "other"))

(define (free-bound-x id)
  (list (free-identifier=? #'x id) (bound-identifier=? #'x id)))

(def-syntax (is-a id)
  (free-identifier=? #'id #'a))

(display
 (let () 
   (list (is-a x) (is-a a))))

(display
 (let* ((a 2) (x a))   
   (list (is-a x) (is-a a))))

(newline)


(display (compare a 1 b a))

(display (let ((a 1) (b 2))
  (compare-ids a b)))

(display (let ((a 1) (b 1))
  (compare-ids a b)))

(display (let ((a 1) (b a))
  (compare-ids a b)))

(display (let ((a 1))
  (compare-ids a a)))
