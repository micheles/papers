#|Comparing identifiers
=================================================

This is the last episode of part V of my Adventures. In the latest
episodes I have discussed various technicalities of Scheme macros,
such as the concepts of syntax object, hygiene and lexical context.
There is still an important subject to be discuss in order to
become really proficient with Scheme macros: *identifier equality*.
Equality of identifiers is one of the trickiest things in
Scheme.

First of all, identifier equality is a compile-time
concept which has nothing to do with the run-time concept of equality
between variables.
Identifiers are not variables: they are syntax objects with an
underlying symbol and an underlying lexical context, which are known
statically at compile time. It is possible to know if an
identifier is bound or not at compile-time, but the value the
identifier will be bound to at run-time is (in general) unknown.

Secondly, there is not a single concept of identifier equality,
but different definitions are possible. In this episode I will
discuss three different predicates to compare identifiers:
``symbol-identifier=?``, ``bound-identifier=?`` and ``free-identifier=?``
(the latter two are part of the R6RS standard).

.. image:: identity.png

.. _22: http://www.artima.com/weblogs/viewpost.jsp?thread=256848

``symbol-identifier=?``
-----------------------------------------------

The simplest concept of identifier equality is expressed by
the following ``symbol-identifier=?`` comparison function
(for convenience, I have added the ``symbol-identifier=?``
precedure to the ``(aps lang)`` library):

$$lang:SYMBOL-IDENTIFIER=?

Two identifiers are ``symbol-identifier=?`` if they are equal as
symbols, once their lexical information has been stripped out.

For instance, ``symbol-identifier=?`` can
be used to find duplicated names in macros defining ``name->value``
tables, such as the ``static-map`` macro I discussed in episode 22_. Moreover, 
``symbol-identifier=?`` can be used to reject reserved
identifiers (you may need such
functionality if are building a mini-language on top of Scheme
and you want to reject a few identifiers as language keywords), as
in the following example:

$$CHECK-RESERVED

``(check-reserved id)`` will raise a syntax-violation if ``id``
is one of the keyword ``reserved1`` or  ``reserved2``.

``bound-identifier=?``
-----------------------------------------------

``symbol-identifier=?`` is simple and easy to understand, but it cannot
be used in all situations. Consider for instance the very first macro
I wrote, in episode 9_:

.. code-block:: scheme

  (def-syntax (multi-define (name1 name2 ...) (value1 value2 ...))
    #'(begin (define name1 value1) (define name2 value2) ...))

It is quite common to write macros defining multiple bindings, such
as ``multi-define``.
``multi-define`` as written does not perform any check for duplicated
identifiers, so that it
relies on the standard behavior of R6RS scheme, raising an error.
However, the standard behavior only applies to programs and scripts,
whereas the REPL is quite free to behaves differently and indeed it does
in most implementations:

.. code-block:: scheme

 > (multi-define (a a) (1 2)); in Ikarus, Ypsilon, ...
 a
 2

(in the REPL latter definitions override previous definitions).
If you are unhappy with that, you can introduce a ``bound-identifier=?`` check
and raise a custom exception:

$$MULTI-DEFINE

Two identifiers are equal according to ``bound-identifier=?`` only if
they have the same name *and* the same marks. The name is
misleading since the arguments of ``bound-identifier=?`` are not
required to be bound identifiers; a better name would be
``strict-identifier=?``.

You can ``check`` that ``multi-define`` correctly reject duplicated
identifiers:

.. code-block:: scheme

 > (multi-define (a a) (1 2))
 Unhandled exception
  Condition components:
    1. &who: multi-define
    2. &message: "Found duplicated identifier in"
    3. &syntax:
        form: (a a)
        subform: #f

In this simple example using ``symbol-identifier=?`` would work too.
However this is not the geneal case. Consider
for instance the following macro expanding to ``multi-define``:

$$MULTI-DEFINE2

``multi-define2`` introduces a dummy identifier ``id2``. 
Had we defined ``multi-define`` in terms of ``symbol-identifier=?``,
calling ``multi-define2`` with argument ``id`` equal to  ``id2``
would have generated a spurious name clash. Fortunately, since we defined
``multi-define`` in terms of ``bound-identifier=?``, nothing bad happens:

.. code-block:: scheme

 > (multi-define2 id2 1)
 id2
 1

``bound-identifier=?`` works in this case because
the identifier ``id2`` introduced by the macro has
different marks from the identifier ``id2`` coming as an argument.

``bound-identifier=?`` is not good for every circumstance. Consider
for instance the following variation of ``multi-define``, featuring
a literal keyword ``as``:

$$MULTI-DEF

This work, but the error messages could stand some improvement.
For instance, if an user misspells the infix identifier ``as``,
she gets a generic ``"invalid syntax"`` error, whereas we
would like to provide a customized error message showing the misspelled
literal identifier. Using ``bound-identifier=?`` we could try to
solve the problem as follows:

$$MULTI-DEF-BAD

Unfortunately this solution does not work at all, since it raises
an error even when the ``as`` identifiers are spelled correctly:

.. code-block:: scheme

 > (multi-def-bad (x as y) (1 as 2))
 Unhandled exception
  Condition components:
    1. &who: multi-def-bad
    2. &message: "Offending infix syntax (expected `as')"
    3. &syntax:
        form: as
        subform: #f
    4. &trace: #<syntax as>

The reason is that ``as`` is not ``bound-identifier=?``
to ``#'as``. We need a less strict comparison predicate.
To this aim the Scheme standard provides another equality procedures
for identifiers, ``free-identifier=?``, which however is not quite right.

``free-identifier=?``
-----------------------------------------

``free-identifier=?`` is
the most complicated equality predicate.
I find its `description in the R6RS document`_ particularly confusing and
the name is misleading since the arguments of
``free-identifier=?`` are not required to be free identifiers.
A better name would be ``lax-identifier=?``.
Two identifiers are ``free-identifier=?`` if

1. they are both bound to the same binding and they share the same name
   (or they shared the same name before renaming at import time);
2. they are both unbound and they share the same name.

In implementations with full phase separation, the identifiers
must also be both bound/unbound in the same phase.
In all other cases the two identifiers are not ``free-identifier=?``.
Here is an example:

.. code-block:: scheme

 > (import (only (aps list-utils) range))
 > (import (rename (aps list-utils) (range r)))
 > (free-identifier=? #'r #'range)
 #t

Notice that both ``symbol-identifier=?`` and ``bound-identifier=?``
would fail to recognize the identity of ``range`` and ``r`` in this
case.

It is important to know about ``free-identifier=?`` because 
in macros with literal identifiers the literal identifiers
are compared by using it, internally. That explain a behavior
which can be quite surprising.

.. _9: http://www.artima.com/weblogs/viewpost.jsp?thread=240804
.. _description in the R6RS document: http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html#node_idx_1142

Literal identifiers and auxiliary syntax
-----------------------------------------------------

Consider the macro ``multi-def`` defined in the previous
paragraph. This works:

.. code-block:: scheme

 > (let ()
    (multi-def (x as 1) (y as 2))
    (list x y))
 (1 2)

But this does not work:

.. code-block:: scheme

 > (let ((as 2))
     (multi-def (x as 1) (y as 2))
     (list x y))
 Unhandled exception
  Condition components:
    1. &message: "invalid syntax"
    2. &syntax:
        form: (multi-def (x as 1) (y as 2))
        subform: #f
    3. &trace: #<syntax (multi-def (x as 1) (y as 2))>

That looks surprising, but it is not once you realize that internally
literal identifiers are compared via  ``free-identifier=?``.
In the second example ``as`` is bound, and therefore
it is not ``free-identifier=?`` to the literal identifier ``#'as``, which
is unbound.

The recommended "solution" is to introduce at top level some dummy definitions
for the literal identifiers you are going to use in your macro, and to export
them. Following this policy,
the R6RS document defines a set of special macros,
``_``, ``...``, ``else`` and ``=>``, which lives in the global
namespace and are available to all R6RS programs.

Such macros are used
as auxiliary syntax in various special forms, like ``cond`` and
``syntax-case``; for this reason they are usually called auxiliary
keywords.  The existence of such global variables makes it impossible
to redefine them at top-level in scripts (but it can be done at the
REPL); however they can be redefined locally, thus breaking the macros
using the auxiliary syntax:

.. code-block:: scheme

 > (let ((else #f))
    (cond (else 'something)))
 > ; does not return something

I think this is fundamentally
broken: literal identifiers should be a concept internal to
the macro and they should not be exported. The mistake is
that the R6RS requires the literal identifiers to be matched
via ``free-identifier=?``, whereas they should be matched
with ``symbol-identifier=?``. I never understood why the editors
decided to use ``free-identifier=?``, perhaps because it makes it
possible to rename the identifiers used as literal identifiers,
a feature that looks of little utility to me. All in all, I think
``free-identifier=?`` is another dark corner of R6RS Scheme.
|#

(import (rnrs) (sweet-macros) (aps compat)
        (for (aps cut) expand)
        (for (aps lang) expand)
        (for (aps list-utils) expand)
        (aps easy-test))

;;STATIC-MAP
(def-syntax (static-map (name value) ...)
  #'(syntax-match (<names> name ...)
      (sub (ctx <names>) #''(name ...))
      (sub (ctx name) #'value)
      ...)
  (distinct? symbol-identifier=? #'(name ...))
  (syntax-violation 'static-map "Found duplicated identifier in"
                    #'(name ...)))
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

(def-syntax (compare-ids id1 id2)
  (let ((res '()))
   (when (symbol-identifier=? #'id1 #'id2)
     (set! res (cons 'raw= res)))
   (when (bound-identifier=? #'id1 #'id2)
     (set! res (cons 'bound= res))) 
   (when (free-identifier=? #'id1 #'id2)
     (set! res (cons 'free= res)))
   (datum->syntax #'compare-ids `',res)))

;;MULTI-DEF
;; not checking for duplicated identifiers here
(define-syntax multi-def
  (syntax-rules (as)
    ((multi-def (name as value) ...)
     (begin (define name value) ...))))
;;END
    
;;MULTI-DEF-BAD
(def-syntax  (multi-def-bad (name as_ value) ...)
  #'(begin (define name value) ...)
  (for-all (lambda (id)
             (when (not (bound-identifier=? id #'as))
               (syntax-violation
                'multi-def-bad "Offending infix syntax (expected `as')" id)))
           #'(as_ ...)))
;;END

;;FREE-DISTINCT
(def-syntax (free-distinct name ...)
  (distinct? free-identifier=? #'(name ...)))
;;END

;;BOUND-DISTINCT
(def-syntax (bound-distinct name ...)
  (distinct? bound-identifier=? #'(name ...)))
;;END

;;SYMBOL-DISTINCT
(def-syntax (symbol-distinct name ...)
  (distinct? symbol-identifier=? #'(name ...)))
;;END

;;CHECK-RESERVED
(def-syntax (check-reserved id)
  (syntax-violation 'check-reserved "Reserved identifier" #'id)
  (exists (cut symbol-identifier=? #'id <>) (list #'reserved1 #'reserved2))
  'non-reserved)
;;END

(def-syntax (distinct-x a)
  #'(symbol-distinct x a))

(run

 (test "reserved" (check-reserved a) #f)
 (test "symbol1" (symbol-distinct a b) #t)
 (test "symbol2" (symbol-distinct a a) #f)
 
 (test "bound1" (bound-distinct a b) #t)
 (test "bound2" (bound-distinct a a) #f)
 (test "free1" (free-distinct a b) #t)
 (test "free2" (free-distinct a a) #f)
 )
