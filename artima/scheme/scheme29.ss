#|Breaking hygiene
==============================================================

In the `previous episode`_ I said that hygienic macros are good, since
they solve the variable capture problem. However, purely hygienic macros
introduce a problem of their own, since they
make it impossible to introduce variables at all.
Consider for instance the following trivial macro:

$$DEFINE-A*

``(define-a x)`` *apparently* expand to ``(define a x)``, so
you may find the following surprising:

.. code-block:: scheme

 > (define-a 1)
 > a
 Unhandled exception
  Condition components:
    1. &undefined
    2. &who: eval
    3. &message: "unbound variable"
    4. &irritants: (a)

Why is the variable ``a`` not bound to 1? The problem is that hygienic
macros *never introduce identifiers implicitly*.
Auxiliary names introduced in a macro *are not visible outside* and the
only names which enter in the expansion are the ones we put in.
A mechanism to introduce identifiers, i.e. a mechanism
to break hygiene, is needed if you want to define binding forms.

.. _previous episode: http://www.artima.com/weblogs/viewpost.jsp?thread=260195
.. _28: http://www.artima.com/weblogs/viewpost.jsp?thread=260195
.. _27: http://www.artima.com/weblogs/viewpost.jsp?thread=260182

``datum->syntax`` revisited
--------------------------------------------

Scheme has a builtin mechanism to break hygiene, and we already saw
it: it is the ``datum->syntax`` utility which converts literal
objects (*datums*) into syntax objects.  I have shown
``datum->syntax`` at work in episodes 27_ and 28_ : it was used there
to convert lists describing source code into syntax objects.  A more
typical use case for ``datum->syntax`` is to turn symbols into proper
identifiers. Such identifiers can then be introduced in macros and
made visible to expanded code.

In order to understand the mechanism, you must always remember
that identifiers in Scheme - in the technical sense of objects
recognized by the ``identifier?`` predicate - are not just raw
symbols, they are syntax objects with lexical information attached to
them. If you want to turn a raw symbol into an identifier you must add
the lexical information to it, and this is done by copying the lexical
information coming from the context object in ``datum->syntax``.

For instance, here is how
you can "fix" the macro ``define-a``:

$$DEFINE-A

The symbol ``'a`` here is being promoted to a *bona fide* identifier,
by adding to it the lexical context associated to the macro name.
You can check that the identifier ``a`` is really introduced as
follows:

.. code-block:: scheme

 > (define-a* 1)
 > a
 1

A more realistic example is to use ``syntax->datum``
to build new identifiers from strings.
For that purpose I have added an ``identifier-append``
utility in my ``(aps lang)`` library, defined as follow:

$$lang:IDENTIFIER-APPEND

Here is a simple ``def-book`` macro using ``identifier-append``:

$$DEF-BOOK

``def-book`` here is just as an example of use of ``identifier-append``,
it is not as a recommended pattern to define records.
There are much better ways to define records in Scheme, as we will see
in part VI of these Adventures.

Anyway, ``def-book`` works as follows.
Given a single identifier ``name`` and two values it
introduces three identifiers 
in the current lexical scope: ``name`` (bound to a vector
containing the two values), ``name-title`` (bound to the
first value) and ``name-author`` (bound to the second value).

.. code-block:: scheme

 > (def-book bible "The Bible" "God")
 > bible
 #("The Bible" "God")
 > bible-title
 "The Bible"
 > bible-author
 "God"

Playing with the lexical context
---------------------------------------------------------------

The lexical context is just the set of
names which are visible to an object in a given lexical position
in the source code. Here is an example of a lexical context which
is particularly restricted:

$$experimental/dummy-ctxt:

The identifier ``#'here`` only sees the names ``define``,
``syntax`` and ``dummy-ctxt``: this is the lexical
context of any object in its position in the source code. Had we not
restricted the import, the lexical context of ``#'here`` would have
been the entire ``rnrs`` set of identifiers. We can use ``dummy-ctxt``
to expand a macro into a minimal context. Here is an example of
a trivial macro expanding into such minimal context:

.. code-block:: scheme

 > (import (experimental dummy-ctxt))
 > (def-syntax expand-to-car
    (lambda (x) (datum->syntax dummy-ctxt 'car)))

The macro ``expand-to-car`` expands to a syntax object obtained by
attaching to the symbol ``'car`` the lexical context ``dummy-ctxt``.
Since in such lexical context the built-in ``car`` is not defined,
the expansion fails:

.. code-block:: scheme

 > (expand-to-car)
  Unhandled exception
   Condition components:
     1. &undefined
     2. &who: eval
     3. &message: "unbound variable"
     4. &irritants: (car)

A similar macro ``expand-to-dummy-ctxt`` instead would succeed since
``dummy-ctxt`` is bound in that lexical context:

.. code-block:: scheme

 > (def-syntax expand-to-dummy-ctxt
     (lambda (x) (datum->syntax dummy-ctxt 'dummy-ctxt)))
 >  (expand-to-dummy-ctxt)
 #<syntax here [char 115 of /home/micheles/gcode/scheme/aps/dummy-ctxt.sls]>

.. image:: you-are-here.png

In the definition of ``define-macro`` I gave
in episode 28_ I used the name of the defined macro as lexical
context. The consequence of this choice is that ``define-macro`` style
macros are expanded within the lexical context of the code
where the macro is invoked. For instance in this example

.. code-block:: scheme

 > (let ((x 42))
    (define-macro (m) 'x) ; (m) should expand to 42
    (let ((x 43))
      (m)))
 43 ; surprise!

``(m)`` expand to 43 since in the lexical context where the macro
is invoked ``x`` is bound to 43. However, this behavior is quite
surprising, and most likely not what it is wanted. This is actually
another example of the free-symbol capture problem. It should be
clear that the capture comes from expanding the macro in the macro-call
context, not in the macro-definition context.

Hygienic vs non-hygienic macro systems
----------------------------------------------

Understanding non-hygienic macros is important if you intend to work
in the larger Lisp world. In the scheme community
everybody thinks that hygiene is an essential feature and
all major Scheme implementations provide hygienic macros; nevertheless,
in the rest of the world things are different.

For instance, Common Lisp does not use hygienic macros and it copes with the
variable capture problem by using ``gensym``; the free symbol
capture problem is not solved, but it is extremely rare, because
Common Lisp has multiple namespaces and a package system.

The hygiene problem is more serious in Lisp-1_ dialects like the
newborns Arc_ and Clojure_.  Arc_ macros behave just like
``define-macro`` and are fully unhygienic, whereas `Clojure macros`_ are
rather special, being nearly hygienic. In particular Clojure
macros are not affected by the free-symbol capture problem:

.. code-block:: scheme

 user=> (defmacro m[x] `(list ~x))
 #'user/m
 user=> (let [list 1] (m 2))
 (2)

The reason is that Clojure is smart enough to recognize the fully
qualified ``list`` object appearing at macro definition
time (``clojure.core/list``) as a distinct object from the local
variable ``list`` bound to the number 1.  Moreover, the ordinary
capture problem can be solved with ``gensym`` or even cooler feature,
automatic gensyms (look at the documentation of the syntax-quote_
reader macro if you want to know more).  Speaking as a
non-expert, Clojure macros seem to fare pretty well with respect to
the hygiene issue.

It is worth mentioning that if you use a package system (like in
Common Lisp) or a namespace system (like in Clojure) in practice
variable capture becomes pretty rare. In Scheme instead, which uses a
module system, hygiene is essential: if you are writing a module
containing macros which can be imported and expanded in an unknown
lexical scope, in absence of hygiene you could introduce name clashes
impossible to foresee in advance, and that could be solved only by the
final user, which however will likely be ignorant of how your library
works.

This is why in Scheme the macro expansion is not literally
inserted in the original code, and a lot of magic takes place to avoid
name clashes. In practice, the implementation of Scheme macros takes
care of distinguishing the introduced identifiers with some specific
mechanism (it could be based on marking the names, or on explicit
renaming). As a consequence, the mechanism of macro expansion is less
simple to explain: you cannot just cut and paste the result
of the expansion in your original code.

Personally I have made my mind up and I am in the pro-hygiene camp
now.  I should admit that for a long time I have been in the opposite
camp, preferring the simplicity of ``define-macro`` over the
complexity of ``syntax-case``. It turns out I was wrong.  The major
problem of ``syntax-case`` is a cosmetic one: it looks very complex
and cumbersome to use, but that can be easily solved by providing a
nicer API - which I did with ``sweeet-macros``. Actually I have
been able to use ``sweet-macros`` for twenty episodes without
explaining the intricacies of the hygienic expansion.

Having attended to a talk on the subject at the `EuroLisp Symposium`_,
I will mention here that there are `ways to implement hygienic
macros`_ on top of ``defmacro`` in Common Lisp *portably*. Therefore
there is no technical reason why hygienic macros are not widespread in
the whole Lisp world, just a matter of different opinions on the
importance of the problem and the different tradeoffs.  I believe that
eventually all Lisp dialects will start using hygienic macros, but
that could take decades, because of inertia and backward-compatibility
concerns.

.. _Clojure macros: http://clojure.org/Macros
.. _Arc: http://arclanguage.org/
.. _Clojure: http://clojure.org/
.. _syntax-quote: http://clojure.org/reader#syntax-quote
.. _Lisp-1: http://en.wikipedia.org/wiki/Lisp-1#The_function_namespace
.. _EuroLisp Symposium: http://www.european-lisp-symposium.org/
.. _ways to implement hygienic macros: http://p-cos.net/documents/hygiene.pdf
|#

(import (rnrs) (sweet-macros) (aps list-utils) (aps lang) (aps easy-test)
        (aps compat))

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

;DEFINE-A*
(def-syntax (define-a x)
  #'(define a x))
;END

;;DEFINE-A
(def-syntax (define-a* x)
  #`(define #,(datum->syntax #'define-a* 'a) x))
;;END

;;DEF-BOOK
(def-syntax (def-book name title author)
  (with-syntax (
     (name-title (identifier-append #'name "-title"))
     (name-author (identifier-append #'name "-author")))
     #'(begin
         (define name (vector title author))
         (define name-title (vector-ref name 0))
         (define name-author (vector-ref name 1)))))
;;END

(pretty-print (syntax-expand (def-book bible "The Bible" "God")))

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
