#|Syntax objects
===================================================================

In the last dozen episodes I have defined plenty of macros, but I have
not really explained what macros are and how they work. This episode
will close the gap, and will explain the true meaning of macros by
introducing the concepts of *syntax object* and of *transformer* over
syntax objects.

Syntax objects
------------------------------------------------------------------

Scheme macros are built over the concept of *syntax object*.
The concept is peculiar to Scheme and has no counterpart in other 
languages (including Common Lisp), therefore it is worth to spend some time
on it.

A *syntax-object* is a kind of enhanced *s*-espression: it contains
the source code as a list of symbols and primitive values, plus
additional informations, such as
the name of the file containing the source code, the line numbers,
a set of marks to distinguish identifiers according to their
lexical context, and more.

It is possible to convert a name or a literal value into a 
syntax object with the syntax quoting operation, i.e. the funny
``#'`` symbol you have seen in all the macros I have defined until now::

 > #'x ; convert a name into an identifier
 #<syntax x>
 > #''x ; convert a literal symbol
 #<syntax 'x>
 > #'1 ; convert a literal number
 #<syntax 1>
 > #'"s" ; convert a literal string
 #<syntax "s">
 > #''(1 "a" 'b) ; convert a literal data structure
 #<syntax '(1 "a" 'b)>

Here I am running all my examples under Ikarus; your Scheme
system may have a slightly different output representation for syntax
objects.

In general ``#'`` - also spelled ``(syntax )`` - can be "applied"
to any expression::

 > (define syntax-expr #'(display "hello"))
 > syntax-expr
 #<syntax (display "hello")>

It is possible to extract the *s*-expression underlying the
syntax object with the ``syntax->datum`` primitive::

 > (equal? (syntax->datum syntax-expr) '(display "hello"))
 #t

Different syntax-objects can be equivalent: for instance the improper
list of syntax objects ``(cons #'display (cons #'"hello" #'()))`` is
equivalent to the syntax object ``#'(display "hello")`` in the sense
that both corresponds to the same datum::

 > (equal? (syntax->datum (cons #'display (cons #'"hello" #'())))
           (syntax->datum #'(display "hello")))
 #t

The ``(syntax )`` macro is analogous to the ``(quote )`` macro;
moreover, there is a ``quasisyntax`` macro denoted with ``#``` which
is analogous to the ``quasiquote`` macro (`````) and, in analogy to
the operation ``,`` and ``,@`` on regular lists, there are two
operations ``unsyntax`` ``#,`` (*sharp comma*) e ``unsyntax-splicing``
``#,@`` (*sharp comma splice*) on lists (including improper lists) of
syntax objects.

Here is an example using sharp-comma::

 > (let ((user "michele")) #`(display #,user))
 (#<syntax display> "michele" . #<syntax ()>)

and here is an example using sharp-comma-splice::

 > (define users (list #'"michele" #'"mario"))
 > #`(display (list #,@users))
 (#<syntax display>
 (#<syntax list> #<syntax "michele"> #<syntax "mario">) . #<syntax ()>)

Notice that the output is an improper list. This is somewhat consistent
with the behavior of usual quoting: for usual quoting ``'(a b c)``
is a shortcut for ``(cons* 'a 'b 'c '())``, which is a proper list,
and for syntax-quoting ``#'(a b c)`` is equivalent to
``(cons* #'a #'b #'c #'())``, which is an improper list.
The ``cons*`` operator here is a R6RS shortcut for nested conses:
``(cons* w x y z)`` is the same as ``(cons w (cons x (cons y z)))``.

However, the result of a quasi quote interpolation is very much
*implementation-dependent*: Ikarus returns an improper list, but other
implementations returns different results; for instance ypsilon
returns a proper list of syntax objects whereas PLT Scheme returns
an atomic syntax object. The lesson is that you cannot
rely on properties of the inner representation of syntax objects:
what matters is the code they correspond to, i.e. the result of
``syntax->datum``.

It is possible to promote a datum to a syntax object with the
``datum->syntax`` procedure, but in order
to do so you need to provide a lexical context, which can be specified
by using an identifier::

 > (datum->syntax #'dummy-context '(display "hello"))
 #<syntax (display "hello")

(the meaning of the lexical context in ``datum->syntax`` is tricky and
I will go back to that in the next episode).

What ``syntax-match`` really is
--------------------------------------------------------------

``syntax-match`` is a general utility to perform pattern matching
on syntax objects; it takes a syntax object in output and returns
another syntax object in output, depending on the patterns, skeletons and guards
used::

 > (define transformer 
     (syntax-match ()
       (sub (name . args) #'name))); return the name as a syntax object

 > (transformer #'(a 1 2 3))
 #<syntax a>

For convenience, ``syntax-match`` also accepts a second syntax
``(syntax-match x (lit ...) clause ...)`` to match syntax expressions
directly, more convenient than using
``((syntax-match (lit ...) clause ...) x)``.
Here is a simple example of usage::

 > (syntax-match #'(a 1 2 3) ()
    (sub (name . args) #'args)); return the args as a syntax object
 #<syntax (1 2 3)>

Here is an example using ``quasisyntax`` and ``unsyntax-splicing``::

 > (syntax-match #'(a 1 2 3) ()
     (sub (name . args) #`(name #,@#'args)))
 (#<syntax a> #<syntax 1> #<syntax 2> #<syntax 3>)

As you see, it easy to write hieroglyphs if you use ``quasisyntax`` 
and ``unsyntax-splicing``. You can avoid that by means of the ``with-syntax``
form::

 > (syntax-match #'(a 1 2 3) ()
     (sub (name . args) (: with-syntax (a ...) #'args #'(name a ...))))
 (#<syntax a> #<syntax 1> #<syntax 2> #<syntax 3>)
 

The pattern variables introduced by ``with-syntax``
are automatically expanded inside the syntax template, without
resorting to the quasisyntax notation (i.e. there is no need for
``#``` ``#,`` ``#,@``).

.. image:: hieroglyphics.jpg

Matching generic syntax lists
--------------------------------------------------------------

The previous paragraphs about syntax objects were a little abstract and
probably of unclear utility (but what would you expect from
an advanced macro tutorial? ;). Here I will be more
concrete and I will provide an example where
``syntax-match`` is used as a list matcher inside a bigger macro.
The final goal is to provide
a nicer syntax for association lists (an association list is just
a non-empty list of non-empty lists). The macro accepts a variable
number of arguments; every argument is of the form ``(name value)`` or
it is a single identifier: in this case latter case it must be
magically converted
into the form ``(name value)`` where ``value`` is the value of the
identifier, assuming it is bound in the current scope, otherwise
a run time error is raised ``"unbound identifier"``. If you try to
pass an argument which is not of the expected form, a compile time
syntax error must be raised.
In concrete, the macro works as follows:

$$TEST-ALIST

``(alist a (b (* 2 a)))`` would raise an error ``unbound identifier a``.
Here is the implementation:

$$ALIST

The expression ``#'(arg ...)`` expands to a list of syntax
objects which are then transformed by is the ``syntax-match`` transformer,
which converts identifiers of the form ``n`` into couples of the form
``(n n)``, whereas it leaves couples ``(n v)`` unchanged, however
by checking that ``n`` is an identifier.

Macros as list transformers
---------------------------------------------------------------------

Macros are in one-to-one correspondence with list transformers, i.e. every
macro is associated to a transformer which converts a list of syntax objects
(the arguments of the macro) into another list of syntax objects (the expansion
of the macro). Scheme itself takes care of converting the input code
into a list of syntax objects (if you wish, internally there is a
``datum->syntax`` conversion) and the output syntax list into code
(an internal ``syntax->datum`` conversion).
The sharp-quote notation in macros is just an abbreviation for the underlying
list: for instance a macro describing function composition

:: 

  (def-syntax (app f g)
    #'(f g))

can be written equivalently also as

::

 (def-syntax (app f g)
   (list #'f #'g))

or

::

 (def-syntax (app f g)
   (cons* #'f #'g #'()))

The sharp-quoted syntax is more readable, but it hides the underlying list
representation which in some cases is pretty useful. This is why
``syntax-match`` macros are much more powerful than ``syntax-rules``
macros.

``sweet-macros`` provide a convenient feature:
it is possible to extract the associated
transformer for each macro defined via ``def-syntax``. For instance,
here is the transformer associated to  the ``define-a`` macro:

.. code-block:: scheme

 > (define tr (define-a <transformer>))
 > (tr (list #'dummy #'1))
 (#<syntax define> #<syntax a> 1)

Notice that the name of the macro (in this case ``define-a`` is ignored
by the transformer, i.e. it is a dummy identifier.

|#
(import (rnrs) (sweet-macros) (aps easy-test) (aps compat)
        (for (aps list-utils) expand) (for (aps record-syntax) expand run))

;;ALIST
(def-syntax (alist arg ...)
  (with-syntax ((
     ((name value) ...)
     (map (syntax-match ()
            (sub n #'(n n) (identifier? #'n))
            (sub (n v) #'(n v) (identifier? #'n)))
          #'(arg ...)) ))
     #'(let* ((name value) ...)
         (list (list 'name name) ...))))
;;END

(def-syntax book (record-syntax title author))
(pretty-print (syntax-expand (record-syntax title author))) (newline)

(define b (vector "T" "A"))
(display (list (book b title) (book b author))) ;; seems an Ypsilon bug
;since this works
;(def-syntax book
;  (syntax-match (title author)
;   (sub (ctx v title) (syntax (vector-ref v 0)))
;   (sub (ctx v author) (syntax (vector-ref v 1)))))

(display (syntax-expand (alist (a 1) (b (* 2 a)))))

(run

 ;;TEST-ALIST
 (test "simple"
       (alist (a 1) (b (* 2 a)))
       '((a 1) (b 2)))
 
 ;;END
 ;(test "with-error"
 ;     (catch-error (alist2 (a 1) (2 3)))
 ;     "invalid syntax")

)



