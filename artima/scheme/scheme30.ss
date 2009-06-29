#|Comparing identifiers
=================================================

Equality of identifiers is one of the trickiest things in R6RS
Scheme, since the meaning of identifier equality in a lexically
scoped language with hygienic macros is non-obvious.

Identifier equality is a compile-time
concept which has nothing to do with the run-time concept of equality
between variables: two variables are equal if their underlying values
are equal, but the value to which a variable is bound is only known
at run-time, therefore identifier equality cannot rely on it.

Identifiers are not variables: they are syntax objects with an
underlying symbol and an underlying lexical context, which are known
statically at compile time. It is possible to know if an
identifier is bound or not at compile-time, but the value the
identifier will be bound to at run-time is unknown.

The Scheme standard define two different equality procedures
for identifiers (``bound-identifier=?`` and ``free-identifier=?``,
with absolutely misleading names); to those, I will add a third one,
``symbol-identifier=?``.

This episode will be spent discussing the subtilities of identifier
equality.

.. image:: Love_equality.png

``symbol-identifier=?``
-----------------------------------------------

The simplest concept of identifier equality is expressed by
the following ``symbol-identifier=?`` comparison function
(for convenience, I have added the ``symbol-identifier=?``
precedure in the ``(aps lang)`` library):

$$lang:SYMBOL-IDENTIFIER=?

Two identifiers are ``symbol-identifier=?`` if they are equal as
symbols.

``symbol-identifier=?`` can
be used to manage duplicate names in macros defining multiple
identifiers at once, or in macros defining name->value tables, such as
the ``static-map`` I discussed in episode 22. Moreover, 
``symbol-identifier=?`` can be used to reject reserved
identifiers (you may need such
functionality if are buildin a mini-language on top of Scheme
and you want to reject a few identifiers as language keywords), as
in the following macro:

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

You may also believe that using ``symbol-identifier=?`` would work too.
However this is not the case, in general. Consider
for instance the following macro expanding to ``multi-define``:

$$MULTI-DEFINE2

The macro introduces a dummy identifier ``id2``. 
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

``free-identifier=?``
------------------------------------

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
        form: (as as)
        subform: #f

The reason is that ``as`` is not ``bound-identifier=?``
to ``#'as``. We need a less strict comparison predicate such as
``symbol-identifier=?`` or ``free-identifier=?``.

``free-identifier=?`` is
the most complicated equality predicate. As
usual, the name is misleading since the arguments of
``free-identifier=?`` are not required to be free identifiers.
A better name would be ``lax-identifier=?``.
Two identifiers are ``free-identifier=?`` if

1. they are both bound and they share the same name, or they shared
   the same name before renaming at import time;
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

.. _9: http://www.artima.com/weblogs/viewpost.jsp?thread=240804

Literal identifiers and auxiliary syntax
-----------------------------------------------------

Macros with literal identifiers may be surprising, because internally
the literal identifiers are compared by using ``free-identifier=?``.
Consider for instance the macro ``multi-def`` defined in the previous
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

The reason is that in the second example ``as`` is bound, and therefore
it is not ``free-identifier=?`` to the literal identifier ``#'as``.

The recommended "solution" is to define at top-level and to
export some dummy definition for the literal identifiers you
are going to use in your macro. I think this is fundamentally
broken: literal identifiers should be a concept internal to
the macro and they should not be exported. The mistake is
that the R6RS requires the literal identifiers to be matched
via ``free-identifier=?``, whereas they should be matched
with ``symbol-identifier=?``. I never understood why the editors
decided to use ``free-identifier=?``, perhaps because it makes it
possible to rename the identifiers used as literal identifiers,
a feature that looks more like a misfeature to me.

Anyway, the R6RS document defines a set of special macros, such as
``_``, ``...``, ``else`` and ``=>``, which lives in the global
namespace and are available to all R6RS programs. Such macros are used
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

(let ((a 1))
  (define id-a #'a)
  (let ((a a))
    (free-identifier=? #'a id-a)))
    
;;MULTI-DEF-BAD
(def-syntax (multi-def-bad (name as_ value) ...)
  #'(begin (define name value) ...)
  (for-all (lambda (id) (bound-identifier=? id #'as)) #'(as_ ...))
  (syntax-violation 'multi-def-bad "Offending infix syntax (expected `as')"
    (remp (lambda (id) (bound-identifier=? id #'as)) #'(as_ ...))))
;;END

;;MULTI-DEF
 (define-syntax multi-def
   (syntax-rules (as)
     ((multi-def (name as value) ...)
        (begin (define name value) ...))))
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
  #f)
;;END

(def-syntax (distinct-x a)
  #'(symbol-distinct x a))

(run
 (test "symbol1" (symbol-distinct a b) #t)
 (test "symbol2" (symbol-distinct a a) #f)
 
 (test "bound1" (bound-distinct a b) #t)
 (test "bound2" (bound-distinct a a) #f)
 (test "free1" (free-distinct a b) #t)
 (test "free2" (free-distinct a a) #f)
 )
