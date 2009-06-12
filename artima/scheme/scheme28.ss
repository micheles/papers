#|Hygienic macros
==============================================================

In episode 9_ I akwnoledged the fact that Scheme provides at least three
different macro systems - ``syntax-rules``, ``syntax-case`` and
``define-macro`` - yet I have
decided to explain only my own personal macro system - ``sweet-macros``.
The decision was motivated by pedagogical reasons,  because I did not want
to confuse my readers by describe too many macro frameworks
at the same time, and also because I want to make macros
easier, by providing a nicer syntax and introspection features.
However now, after more than a dozen
episodes about macros, I can assume my readers are beginners no more,
and it is time to have a look at the larger Scheme world and to
compare/contrast ``sweet-macro`` with the most used systems, i.e.
``syntax-rules``, ``syntax-case`` and ``define-macro``.

Actually, there a few other intesting options
out there, such as syntactic closures and systems based on explicit
renaming. Since I do not want to discuss all the macro systems in existence
here, I will point out a good resource on the
subject: this excellent `post by Alex Shinn`_ on the Chicken mailing list
summarizes the situation better than I could do.

``syntax-match`` vs ``syntax-rules``
-----------------------------------------------------------------

``syntax-rules`` can be quite trivially defined in terms of
``syntax-match``::

 (def-syntax (syntax-rules (literal ...) (patt templ) ...)
   #'(syntax-match (literal ...) (sub patt #'templ) ...))

As you see, the difference between ``syntax-rules`` (a part for
missing the ``sub`` literal) is that ``syntax-rules`` automatically
adds the syntax-quote ``#'`` operator to you templates. That means
that you cannot use quasisyntax tricks and that ``syntax-rules`` is
strictly less powerful than ``syntax-match``. Moreover,
``syntax-rules`` macros do not have guarded patterns; the most direct
consequence is that providing good error messages for wrong syntaxes
is more difficult.

``syntax-match`` vs ``syntax-case``
-----------------------------------------------------------------

``syntax-case`` could be defined in terms of ``syntax-match`` as
follows::

 (def-syntax syntax-case
   (syntax-match ()
    (sub (syntax-case x (literal ...) (patt guard skel) ...)
     #'(syntax-match x (literal ...) (sub patt skel guard) ...))
    (sub (syntax-case x (literal ...) (patt skel) ...)
     #'(syntax-match x (literal ...) (sub patt skel) ...))
  ))

In reality, ``syntax-case`` is a Scheme primitive and
``syntax-match`` is defined on top of it. So, ``syntax-case`` has
theoretically the same power as ``syntax-match``, but in practice
``syntax-match`` is more convenient to use because of the
introspection features.

The major syntactic difference (apart from the absense of the ``sub`` literal)
is the position of the guard, which in ``syntax-case`` is positioned *before*
the skeleton, whereas in ``syntax-match`` is positioned *after* the skeleton.
Changing the position has cost me a lot of reflection, since I *hate*
gratuitous breaking. However, I am convinced that the position of the
guard in ``syntax-case`` is really broken, so I had to *fix* the issue.

XXX: why do I say so?

``syntax-match`` versus ``define-macro``
---------------------------------------------------------------

Nowadays macros based on ``define-macro`` are much less used than in
past because macro systems based on pattern matching are much more
powerful, easier and safer to use. The R6RS specification made
``syntax-case`` enter in the standard and this is the preferred macro
system for most implementation.

Nowadays, there is a good chance that your Scheme implementation does
not provide ``define-macro`` out of the box, therefore you need to
implement it in term of ``syntax-case`` (or ``syntax-match``). Here is
an example of such an implementation:

$$DEFINE-MACRO

The code should be clear: the arguments of the macro are converted into
a regular list which is then transformed with the expander, and converted
back into a syntax object in the context of the macro. 

.. _9: http://www.artima.com/weblogs/viewpost.jsp?thread=240804
.. _On Lisp: http://www.paulgraham.com/onlisp.html
.. _hygiene in R6RS: http://docs.plt-scheme.org/r6rs-lib-std/r6rs-lib-Z-H-13.html#node_sec_12.1
.. _post by Alex Shinn: http://lists.gnu.org/archive/html/chicken-users/2008-04/msg00013.html

The hygiene problem
---------------------------------------------------------------------

If you have experience in Common Lisp or other Lisp dialects, you will
have heard about the problem of hygiene in macros, a.k.a. the problem
of *variable capture*. In Scheme you have the same problem if you use
``define-macro``. The issue is that having macros
introducing identifiers implicitly can cause unespected side
effects. As Paul Graham puts it,
*errors caused by variable capture are rare, but what they lack
in frequency they make up in viciousness*.

The hygiene problem is the main reason why `define-macro`` is becoming
less and less used in the Scheme world. PLT Scheme has being
deprecating it for many years and nowadays even Chicken Scheme, which
traditionally used ``define-macro`` a lot, has removed it from the
core, by using hygienic macros instead: this is the reason why the
current Chicken (Chicken 4.0) is called "hygienic Chicken".

You can find good discussions of the hygiene problem in Common Lisp
in many places; I am familiar with Paul Graham's book `On Lisp`_ which
I definitively recommend: the chapter on variable chapter is the best
reference I know. Another good reference is the chapter
about ``syntax-case`` - by Kent Dybvig - in the book `Beautiful Code`_.
Here I will give just a short example exhibiting the problem, for the
sake of the readers unfamiliar with it.

.. image:: hygienic-paper-small.jpg

Consider for instance this "dirty" definition of the ``for`` loop:

.. code-block:: scheme

 (define-macro (dirty-for i i1 i2 . body)
   `(let ((start ,i1) (stop ,i2))
      (let loop ((i start))
        (unless (>= i stop) ,@body (loop (+ 1 i))))))

The mistake here is having forgotten to ``gensym`` the newly
introduced variables ``start`` and ``stop``, a common mistake for
beginners (and occasionally, even for non beginners). That means that
the macro is not safe under variable capture and indeed code such as

.. code-block:: scheme

 > (let ((start 42))
    (dirty-for i 1 3 (print start)))
 11

prints twice the number 1 and not the number 42. On the other hand,
everything works fine if the ``for`` macro is defined using
``def-syntax``.  There is no doubt that ``def-syntax`` is
nicer/simpler to writer than ``define-macro`` *and* much less error
prone.

The other problem with non-hygienic macros is that introduced identifiers
will have the scope of expanded code, not the scope of the original macro:
that means that if the outer scope redefines the meaning of an
identifier used internally, the macro will work in an unexpected way.
Consider for instance the following expression:

> (let ((unless 'unless))
    (dirty-for i 1 3 (print i)))

There is an error here because shadowing ``unless`` affects the
``dirty-for`` macro.  This is pretty tricky to debug: in practice, it
means that the macro user is forced to know all the identifiers
that are used internally by the macro.

.. _Beautiful Code: http://oreilly.com/catalog/9780596510046/

Breaking hygiene
-------------------------------------------------

If you only use hygienic macros, the hygiene probleme does not exist.
However, there is the opposite problem: you need a way of breaking
hygiene on purpose.  Consider for instance the following apparently
trivial macro:

.. code-block:: scheme

 (def-syntax (define-a x)
   #`(define a x))

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
new names in a macro is called *breaking hygiene*.

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
|#

(import (rnrs) (sweet-macros) (aps lang) (aps list-utils) (aps compat)
        (aps test-utils))

;DEFINE-MACRO
(def-syntax define-macro
  (syntax-match ()
     (sub (define-macro (name . params) body1 body2 ...)
       #'(define-macro name (lambda params body1 body2 ...)))
     (sub (define-macro name expander)
       #'(def-syntax (name . args)
          (datum->syntax #'name (apply expander (syntax->datum #'args)))))
     ))
;END

;;DEFINE-A
(def-syntax (define-a x)
  #`(define #,(datum->syntax #'define-a 'a) x))
;;END

;DEFINE-A-NH
(define-macro (define-a* x)
  `(define a ,x))
;END

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