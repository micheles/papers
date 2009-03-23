#|
Relation with standard macros
---------------------------------------------------------------

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
compare/contrast ``sweet-macro`` with the other systems out there.

syntax-match vs syntax-rules
-----------------------------------------------------------------

``syntax-rules`` can be quite trivially defined in terms of
``syntax-match``::

 (def-syntax (syntax-rules (literal ...) (patt templ) ...)
   #'(syntax-match (literal ...) (sub patt #'templ) ...))

As you see, the difference between ``syntax-rules`` (a part
for missing the ``sub`` literal)
is that ``syntax-rules`` automatically adds the syntax-quote ``#'``
operator to you templates. That means that you cannot use
quasisyntax tricks and that ``syntax-rules`` is strictly less
powerful than ``syntax-match``: in particular you cannot
break hygiene with it. Moreover, ``syntax-rules`` macros
do not have guarded patterns; the most direct consequence is that
providing good error messages for wrong syntaxes is more difficult.

syntax-match vs syntax-cases
-----------------------------------------------------------------

``syntax-case`` can also be defined in terms of ``syntax-match`` as follows::

 (def-syntax syntax-case
   (syntax-match ()
    (sub (syntax-case x (literal ...) (patt guard skel) ...)
     #'(syntax-match x (literal ...) (sub patt skel guard) ...))
    (sub (syntax-case x (literal ...) (patt skel) ...)
     #'(syntax-match x (literal ...) (sub patt skel) ...))
  ))

In practice, however, ``syntax-case`` is a Scheme primitive and
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
Why do I say so?

``syntax-match`` versus ``define-macro``
---------------------------------------------------------------

Nowadays macros based on ``define-macro`` are much less used than in past,
in part because of the hygiene issue, and in part because macro systems based
on pattern matching are much more powerful and easy to use. The R6RS
specification made ``syntax-case`` enters in the standard and this is
the preferred macro system for most implementation.  For instance
Chicken Scheme, which traditionally used ``define-macro`` a lot, is
going to remove it from the core, using ``syntax-case`` instead: this
is the reason why the next major Chicken version, Chicken 4.0, will be
called "hygienic Chicken".

Nowadays, there is a good chance that your Scheme implementation does
not provide ``define-macro`` out of the box, therefore you need to implement
it in term of ``syntax-case`` (or ``syntax-match``). Here is an example
of such an implementation:

$$DEFINE-MACRO

The code should be clear: the arguments of the macro are converted into
a regular list which is then transformed with the expander, and converted
back into a syntax object in the context of the macro. The problem with
``define-macro`` (and the reason it is becoming less and less used in
the Scheme world) is the hygiene problem.

.. _9: http://www.artima.com/weblogs/viewpost.jsp?thread=240804

More on the hygiene problem
-------------------------------------------------

I have already said a few words about hygiene last week.
If you have experience in Common Lisp or other Lisp dialects, you will
have heard about the problem of hygiene in macros. You can find good
discussions of the hygiene problem in Common Lisp in many places; I am
familiar with Paul Graham's book `On Lisp`_ which I definitively
recommend. In Scheme such
problem usually does not exist, but it is worth to know about it, since
often people wants to break hygiene on purpose.

The question is: why I see that this behavior is a problem? It looks like
the natural things to do, isn't it? The issue is that having macros
introducing identifiers implicitly can cause unespected side
effects. The problem is called variable capture. As Paul Graham puts it,
"viciousness".

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
everything works fine if the ``for`` macro is defined using ``def-syntax``.
There is no doubt that ``def-syntax`` is nicer/simpler to writer
than ``define-macro`` *and* much less error prone.

bound-identifier=? and free-identifier=?
=======================================================================

The question if two identifiers refers to the same binding or not

$$EXPAND-X

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

;DEFINE-A
(def-syntax (define-a x)
  #'(define a x))
;END

;DEFINE-A-NH
(define-macro (define-a* x)
  `(define a ,x))
;END

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


