#|
A new cycle of *Adventures* begins. The subject is macros, again.

In the last ten episodes I defined plenty of macros, but I did
not really explain what macros are and how they work.
This episode will close the gap,
and will explain the true meaning of macros by introducting the concept
of *syntax object* and the concept of *transformer* over syntax objects.


We just
scratched the surface of what macros are and can do. The third cycle
will be entirely devolved to the most sophisticated aspects of macros.

A new cycle of *Adventures* begins. The subject is macros, again.

Syntax objects
------------------------------------------------------------------

Scheme macros are based on the concept of *syntax object*.
This concept is peculiar to Scheme and has no counterpart in other 
languages (including Common Lisp), therefore it is worth to spend some time
on it.

A *syntax-object* is a kind of enhanced *s*-espression: it contains
the source code as a list of symbols and primitive values, plus
additional informations, such as
the name of the file containing the source code, the line numbers,
a system of marks to distinguish identifiers according to their
lexical context, and more.

It is possible to convert an identifier or a primitive value into a 
syntax object with the syntax quoting operation, i.e. the funny
``#'`` symbol you have seen in all the macros I have defined until now::

 > #'x ; convert an identifier into a syntax-object identifier
 #<syntax x>
 > #''x ; convert a literal symbol
 #<syntax 'x>
 > #'1 ; convert a literal number
 #<syntax 1>
 > #''(1 2) ; convert a literal data structure
 #<syntax '(1 2)>

Notice that I am running all my examples under Ikarus; your Scheme
system may have a slightly different output representation for syntax
objects.

In general ``#'`` can be applied to any expression::

 > (define syntax-expr #'(display "hello"))
 > syntax-expr
 #<syntax (display "hello")>

It is possible to extract the *s*-expression underlying the
syntax object with the ``syntax->datum`` primitive::

 > (equal? (syntax->datum syntax-expr) '(display "hello"))
 #t

Different syntax-objects can be equivalent: for instance
the improper list of syntax objects ``(cons #'display (cons #'"hello" #'()))``
is equivalent to the syntax object ``#'(display "hello")`` in
the sense that both corresponds to the same datum::

 > (equal? (syntax->datum (cons #'display (cons #'"hello" #'())))
           (syntax->datum #'(display "hello")))
 #t

It is possible to promote a datum to a syntax objects with the
``datum->syntax`` procedure, but in order
to do so you need to provide a lexical context, which can be specified
by using a dummy identifier::

 > (datum->syntax #'dummy-context '(display "hello"))
 #<syntax (display "hello")

(the meaning of the lexical context in ``datum->syntax`` is tricky and
I will go back to that in future episodes).

In analogy to the ``quasiquote`` concept, there is a 
``quasisyntax`` operator denoted with ``#```; moreover, in analogy
to the operation ``,`` and ``,@`` on regular lists, there are two operations
``unsyntax``
``#,`` (*sharp comma*) e ``unsyntax-splicing`` ``#,@`` (*sharp comma splice*)
on lists (inclusing improper lists) of syntax objects. Here is an example
using ``#,``::

 > (let ((user "michele")) #`(display #,user))
 (#<syntax display> "michele" . #<syntax ()>)

and here is an example using ``#,@``::

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
``datum->syntax``.

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
``(syntax-match x () clause ...)`` to match syntax expressions
directly, more convenient than using ``((syntax-match () clause ...) x)``.
Here is a simple example of usage::

 > (syntax-match #'(a 1 2 3) ()
    (sub (name . args) #'args)); return the args as a syntax object
 #<syntax (1 2 3)>

Here is an example using ``quasisyntax`` and ``unsyntax-splicing``::

 > (syntax-match #'(a 1 2 3) ()
     (sub (name . args) #`(name #,@#'args)))
 (#<syntax a> #<syntax 1> #<syntax 2> #<syntax 3>)

As you see, it easy to write hieroglyphs if you use ``quasisyntax`` 
and ``unsyntax-splicing``. To avoid that, Scheme provides a ``with-syntax``
form::

 > (syntax-match #'(a 1 2 3) ()
     (sub (name . args) (with-syntax (((a ...) #'args))
                           #'(name a ...))))
 (#<syntax a> #<syntax 1> #<syntax 2> #<syntax 3>)
 

``with-syntax`` allow to introduce a set of pattern variables which
are automatically expanded inside the syntax template, without
resorting to the quasisyntax notation (i.e. there is no need for
``#```, ``#,``, ``#,@``).

A concrete example
-----------------------------------------------------

The previous paragraphs about syntax objects have been a bit abstract and
probably of unclear utility (but what would you expect from
an advanced macro tutorial? ;). In this paragraph I will be more
concrete and I will provide an useful example of a macro providing
a nicer syntax for association lists (an association list is just
a non-empty list of non-empty lists). The macro will accepts a variable
number of arguments; every argument will be be of the form ``(name value)`` or
just a single identifier: in this case it will be magically converted
into the form ``(name value)`` where ``value`` is the value of the
identifier, assuming it is bound in the current scope, otherwise
a run time error will be raised ("unbound identifier"). If you try to
pass an argument which is not of the expected form, a compile time
syntax error will be raised.
In concrete, the macro will work as follows:

$$TEST-ALIST

``(alist a (b (* 2 a)))`` will raise an error ``unbound identifier a``.
Here is the implementation:

$$ALIST

As you see the core of the macro is the ``syntax-match`` transformer,
which is able to convert identifiers of the form ``n`` into couples ``(n n)``,
whereas it leaves couples ``(n v)`` unchanged, but checking that ``n`` is an
identifier. ``with-syntax`` introduces a set of pattern variables
``(name value)`` which are later used in the macro template, the ``let*``
form.

There are a few things that could be improved with this macro. The
first one is mostly cosmetic: ``with-syntax`` has somewhat too many
parenthesis (it is easy to miss one in ``((((name value) ...) lst))``).
This can be solved which a macro like the following one::

 (def-syntax local
   (syntax-match ()
      (sub (local expr)
           #'expr)
      (sub (local (let-form name value) (l n v) ... expr)
       #'(let-form ((name value)) (local (l n v) ... expr)))))

``local`` is an example of second order macro, since it expects as
argument another macro, in this case the ``let-form``, which can
be any binding macro such that ``(let-form ((name value)) expr)``
is a valid syntax. ``with-syntax`` is a kind of let form, 
so that ``(with-syntax ((((name value) ...) lst)) expr)``
can be rewritten as ``(local (with-syntax ((name value) ...) lst) expr)``,
by reducing four consecutive parenthesis to just two.
Notice that the ``sweet-macros`` module version 0.5 provides a ``local`` form
which plays well with ``syntax-match``, so you are invited to make use of it.

The second problem with ``alist`` is that it would look nicer to
give a name to the logic of management of the arguments:

$$NORMALIZE

Having defined this helper function, the original macro becomes more readable::

 (def-syntax (alist arg ...)
   (local (with-syntax ((name value) ...) (normalize #'(arg ...)))
    #'(let* ((name value) ...)
        (list (list 'name name) ...))))

The major advantage, however, is that now you can reuse the argument
normalization procedure in other macros. To this aim it is enough
to save the ``normalize`` procedure in a separated module and import it
when needed. This is however quite nontrivial, since it involves
a hairy discussion of compile-time vs run-time and implementation-specific
caveats that will take a whole episode to explain in detail. That
will be the subject of episode 20. See you next time!

|#
(import (rnrs) (sweet-macros) (aps list-utils) (aps easy-test))

;;ALIST
(def-syntax (alist arg ...)
  (with-syntax ((((name value) ...)
                 (list-of (syntax-match a ()
                            (sub n #'(n n) (identifier? #'n))
                            (sub (n v) #'(n v) (identifier? #'n)))
                          (a in #'(arg ...)))))
    #'(let* ((name value) ...)
        (list (list 'name name) ...))))
;;END

;;NORMALIZE
(define (normalize ls)
  (list-of (syntax-match a ()
              (sub n #'(n n) (identifier? #'n))
              (sub (n v) #'(n v) (identifier? #'n)))
           (a in ls)))
;;END

(display (syntax-expand (alist (a 1) (b (* 2 a)))))

(run

 ;;TEST-ALIST
 (test "simple"
       (alist (a 1) (b (* 2 a)))
       '((a 1) (b 2)))
 (let ((a 1))
   (test "mixed"
         (alist a (b (* 2 a)))
         '((a 1) (b 2))))

 ;;END
 )
