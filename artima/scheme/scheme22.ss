#|Syntax objects, hygiene and pattern matching
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

Different syntax-objects can be equivalent: for instance
the improper list of syntax objects ``(cons #'display (cons #'"hello" #'()))``
is equivalent to the syntax object ``#'(display "hello")`` in
the sense that both corresponds to the same datum::

 > (equal? (syntax->datum (cons #'display (cons #'"hello" #'())))
           (syntax->datum #'(display "hello")))
 #t

It is possible to promote a datum to a syntax object with the
``datum->syntax`` procedure, but in order
to do so you need to provide a lexical context, which can be specified
by using an identifier::

 > (datum->syntax #'dummy-context '(display "hello"))
 #<syntax (display "hello")

(the meaning of the lexical context in ``datum->syntax`` is tricky and
I will go back to that in future episodes).

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
form introduced in the previous episode::

 > (syntax-match #'(a 1 2 3) ()
     (sub (name . args) (: with-syntax (a ...) #'args #'(name a ...))))
 (#<syntax a> #<syntax 1> #<syntax 2> #<syntax 3>)
 

The pattern variables introduced by ``with-syntax``
are automatically expanded inside the syntax template, without
resorting to the quasisyntax notation (i.e. there is no need for
``#```, ``#,``, ``#,@``).

Example 1: breaking hygiene
--------------------------------------------------------------

The previous paragraphs about syntax objects have been a little abstract and
probably of unclear utility (but what would you expect from
an advanced macro tutorial? ;). In this paragraph I will be more
concrete and I will provide an useful example of usage for ``datum->syntax``.

The typical use case for ``datum->syntax`` is to turn symbols
into proper identifiers which can be introduced in macros and made
visible to expanded code, thus breaking hygiene. Coming back
to the example in the latest issue, the ``def-book`` macro,
we can introduce two identifiers for the fields ``title`` and
``author`` as follows:

$$DEF-BOOK

where the helper function ``identifier-append`` is defined as

$$lang:IDENTIFIER-APPEND

All the functions used here (``string->symbol``, ``string-append``,
``symbol->string`` work in the obvious way. Notice that for convenience
I have put ``identifier-append``, together with a companion function
``identifier-prepend`` in the ``aps`` package, in the ``(aps lang)`` module.

Here is a test, showing that hygiene is effectively broken and that
the identifiers ``name-title`` and ``name-author`` are really introduced
in the namespace after expansion:

$$TEST-DEF-BOOK

**Warning**: if you have a macro depending on helper functions, like
the previous one, you must put the helper functions in a separated
module if you want to ensure portability. Moreover, you must
import the helper functions with the syntax ``(for (module-name) expand)``
meaning that the helper functions are intended to be used at expand
time, in macros. Ikarus is quite forgiving and can just use a regular
import, but PLT Scheme and Larceny will raise an error if you do not
use the ``for expand``. A full description of the module system, with
all the gory details, will require six more episodes, and will constitute
part V of these *Adventures*.

Example 2: matching generic syntax lists
--------------------------------------------------------------

In this paragraph I will show an example of ``syntax-match``, used
at its fullest potential to define a macro providing
a nicer syntax for association lists (an association list is just
a non-empty list of non-empty lists). The macro will accepts a variable
number of arguments; every argument will be be of the form ``(name value)`` or
just a single identifier: in this case it will be magically converted
into the form ``(name value)`` where ``value`` is the value of the
identifier, assuming it is bound in the current scope, otherwise
a run time error will be raised (``"unbound identifier"``). If you try to
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
identifier. 

|#
(import (rnrs) (sweet-macros) (aps easy-test) (aps compat)
        (for (aps lang) expand run))

;;ALIST
(def-syntax (alist arg ...)
  (: with-syntax
     ((name value) ...)
     (map (syntax-match ()
            (sub n #'(n n) (identifier? #'n))
            (sub (n v) #'(n v) (identifier? #'n)))
          #'(arg ...))
     #'(let* ((name value) ...)
         (list (list 'name name) ...))))
;;END

(display (syntax-expand (alist (a 1) (b (* 2 a)))))

;;DEF-BOOK
(def-syntax (def-book name title author)
  (: with-syntax
     name-title (identifier-append #'name "-title")
     name-author (identifier-append #'name "-author")
     #'(begin
         (define name (vector title author))
         (define name-title (vector-ref name 0))
         (define name-author (vector-ref name 1)))))

;;END
(pretty-print (syntax-expand (def-book bible "The Bible" "God")))


(run

 ;;TEST-ALIST
 (test "simple"
       (alist (a 1) (b (* 2 a)))
       '((a 1) (b 2)))
 
 ;;END
 ;(test "with-error"
 ;     (catch-error (alist2 (a 1) (2 3)))
 ;     "invalid syntax")


 ;;TEST-DEF-BOOK
 (test "def-book"
       (let ()
         (def-book bible "The Bible" "God")
         (list bible-title bible-author))
       (list "The Bible" "God"))
 ;;END
)



