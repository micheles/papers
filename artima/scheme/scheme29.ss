#|Comparing identifiers
==============================================================


Hygienic vs non-hygienic macro systems
----------------------------------------------

Understanding the hygiene issue is important if you intend to work
in the larger Lisp world. In the small Scheme world
everybody thinks that hygiene is an essential feature and nowadays
all major Scheme implementations provide hygienic macros;
however, in the rest of the world things are different.

Common Lisp does not use hygienic macros and cope with the
variable capture problem by using ``gensym``; the free symbol
capture problem is not solved, but it is rare. Also the fact
that Common Lisp has multiple namespaces and a package system
helps to mitigate the issue.

The hygiene problem is more
serious in Lisp-1_ dialects like the newborn Arc_ and Clojure_:
still their authors, which are extremely competent programmers,
have decided not to use hygienic macros and to rely on various workarounds.

Personally I have made my mind up and I am in the pro-hygiene camp now.
I should admit that for a long time I have been in the opposite
camp, preferring the simplicity of ``define-macro`` over
the complexity of ``syntax-case``. It turns out I was wrong.
The only problem of ``syntax-case`` is a cosmetic one: it
looks very complex and cumbersome to use, but that can be easily
solved by providing a nicer API - which I did with ``sweeet-macros``.
I believe that
eventually all Lisp dialects will start using hygienic macros, but
that could take decades, because of inertia and backward-compatibility
concerns.

Having attended to a talk on the subject at the `EuroLisp
Symposium`_, I will mention here that Pascal Costanza has found a way to
implement hygienic macros on top of ``defmacro`` in Common
Lisp *portably*. There is no technical reason why
hygienic macros are not widespread in the whole Lisp world,
just a matter of different opinions on the importance of
the problem and the different tradeoffs.


.. _Arc: http://arclanguage.org/
.. _Clojure: http://clojure.org/
.. _Lisp-1: http://en.wikipedia.org/wiki/Lisp-1#The_function_namespace
.. _EuroLisp Symposium: http://www.european-lisp-symposium.org/

The hygiene problem (II)
-------------------------------------------------

However, there is the opposite problem: you need a way of breaking
hygiene on purpose.  Consider for instance the following apparently
trivial macro:

$$DEFINE-A

``(define-a x)`` *apparently* should expand to ``(define a x)``, so
you may find the following surprising::

 > (define-a 1)
 > a
 Unhandled exception
  Condition components:
    1. &undefined
    2. &who: eval
    3. &message: "unbound variable"
    4. &irritants: (a)

Why is the variable ``a`` not defined? The reason is that Scheme
macros are *hygienic*, i.e. they *do not introduce identifiers
implicitly*.  Auxiliary names introduced in a macro *are not visible
outside*: the only names which enter in the expansion are the ones we
put in.

This is a major difference with respect to Common Lisp macros.  In
Common Lisp the mechanism of macro expansion is much simpler to
explain, since the expansion is literal: you could just cut and paste
the result of the expansion in your original code. In Scheme instead
the expansion is not literally inserted in the original code, and a
lot of magic takes place to avoid name clashes. In practice, the
implementation of Scheme macros takes care of distinguishing the
introduced identifiers with some specific mechanism (it
could be based on marking the names, or on explicit renaming).

Once you get used to the idea that the expansion is not
literal, and that all the identifiers internally used by a macro
are opaque unless they are explicitly marked as visible, you will
see the advantages of hygiene.

For instance, if you are writing a library which can be imported
in an unknown environment, in absence of hygiene you could introduce
name clashes impossible to foresee in advance, and that could be solved
only by the final user, which however will likely be ignorant of how
your library works.

To be fair, I should
remark that in Common Lisp there
are ways to work around the absence of hygiene;
nevertheless I like the Scheme way
better, since by default you cannot introduce unwanted names. If
want to introduce new names you can, but you must say so. Introducing
new names in a macro is called *breaking hygiene*. Since I like
to keep you in suspense, I will close this episode here, and I will
make you wait for the next one to discover how to break hygiene ;-)

Breaking hygiene
--------------------------------------------

Scheme provides a builtin mechanism to break hygiene, via the
``datum->syntax`` utility which converts
literal objects (*datums*) into syntax objects. 

I have already shown ``datum->syntax``
at work in the definition of ``define-macro`` from ``syntax-match``,
where it was used to convert a list describing source code into a syntax
object. 

A typical use case for ``datum->syntax`` is to turn symbols
into proper identifiers which can be introduced in macros and made
visible to expanded code, thus breaking hygiene. Here is how
you can "fix" the macro ``define-a``:

$$DEFINE-A

Notice that I have used the name of the macro as the context identifier
in ``datum->syntax``. This is the common practice, but any dummy
identifier would have worked for this example. You can check that
the identifier ``a`` is really introduced as follows:

.. code-block:: scheme

 > (define-a 1)
 > a
 1

A more realistic example is to build identifiers from strings and
symbols. For that purpose I have added an ``identifier-append``
utility in my ``(aps lang)`` library, defined as follow:

$$lang:IDENTIFIER-APPEND

Here is a simple ``def-book`` macro using ``identifier-append``:

$$DEF-BOOK

to be used as in the following test:

$$TEST-DEF-BOOK

There are better ways to define records in Scheme, and there is also
a built-in facility to define record types: you should consider
``def-book`` just as an example of use of ``identifier-append``,
not as a recommended pattern to define records.

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

Moreover you can use ``free-identifier=?`` to compare two unbound
identifiers or a bound and an unbound identifier. It returns true only
if the identifiers share the same name and the same bound/unbound state.
You can leverage on this feature to define a function which is able
to determine if an identifier is bound or unbound:

$$lang:UNBOUND?

Using ``unbound?`` it is easy to write code that performs a certain
action only if a given identifier is bound:

$$WHEN-BOUND

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

;;WHEN-BOUND
(def-syntax (when-bound (id) e* ...)
  #'#f (unbound? #'id) #'(begin e* ...))
;;END
;;DEFINE-A
(def-syntax (define-a x)
  #`(define #,(datum->syntax #'define-a 'a) x))
;;END

;;DEF-BOOK
(def-syntax (def-book name title author)
  (: with-syntax
     name-title (identifier-append #'name "-title")
     name-author (identifier-append #'name "-author")
     #'(begin
         (define inner-name (vector title author))
         (define name-title (vector-ref inner-name 0))
         (define name-author (vector-ref inner-name 1)))))

;;END

;(pretty-print (syntax-expand (def-book bible "The Bible" "God")))

 ;;TEST-DEF-BOOK
 (test "def-book"
       (let ()
         (def-book bible "The Bible" "God")
         (list bible-title bible-author))
       (list "The Bible" "God"))
 ;;END

(def-syntax (is-name x)
  #'(begin (display 'x) (display " is a name\n"))
  (identifier? #'x)
  #'(begin (display 'x) (display " is not a name\n")))
  
(def-syntax (let-3 name list-3 body body* ...)
  #`(let+ ((x y z) list-3)
          (let ((#,(identifier-append #'name  ".x") x)
                (#,(identifier-append #'name  ".y") y)
                (#,(identifier-append #'name  ".z") z))
            body body* ...)))
 
(pretty-print (syntax-expand             
  (let-3 v '(a b c)
   (display (list v.x v.y v.z)))
))


(let-3 v '(a b c)
   (display (list v.x v.y v.z)))
(newline)
