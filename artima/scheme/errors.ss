#|
Statements versus expressions: the functional way
----------------------------------------------------------------

One of the substantial differences between an imperative language
and a functional one is the lack of statements in the latter.

There are least two reasons
for this lack.
One is just a matter of economy: since expressions
are enough, why should we introduce an additional concept?
As we know, functional languages have a monastic vocation and they
try to remove as many concepts as possible.

The other reason is that since a statement cannot return anything
(after all, this is part of the definition of statement), the only way
the statement has to have an effect on the surrounding code is way
side effects and we all know that functional languages dislike side
effects, therefore they dislike statements.

The final outcome is that all the constructs which in other languages
are implemented as statements in a functional language must be
implemented as expressions. In this episode I will give a concrete
example, explaining how the ``try .. except .. finally`` construct of
Python can be translated into Scheme.

Error management in Scheme
-------------------------------------------------------------

One of the major new things in the R6RS specification was the
stardardization of a condition system. The
condition system is not a new concept, since Common Lisp has featured
a condition system for years and many R5RS Scheme implementation
provided equivalent systems. Nevertheless, conditions systems are more
or less unknown outside the Lisp family of languages.

If you have no idea of what "condition system" means, think of it
like an ordinary exception system, with two differences:
exceptions are described by records and not by classes, 
and there is a special class of exceptions which are resumable.
That means that
the control flow, in specific circumnstances - basically when
the condition is a mild error that can be recovered - can re-enter
at the point the condition was raised and continue from there.
This is impossible in Python and in most languages.

The condition mechanism is pretty
complex and I would need an entire episode to discuss it in detail.
Moreover, I cannot discuss it right now, since I have not introduced
the concepts underlying it, i.e. records and continuations.

Records are relatively easy to explain, since they are more or
less similar to records in othe languages.
Since Scheme is not expecially object oriented, it should not come
as a surprise that exceptions
(more correctly called conditions) are implemented as records
and not as classes; moreover, since Scheme records
features inheritance, they are enough to implement a hierarchical
system of conditions.

On the other hand, continuations are pretty difficult to explain
and I will not even attempt
to start here: therefore I will ignore completely resumable exceptions
for the time being.
I will focus on non-resumable conditions, i.e. exceptions, instead.

The standard specifies an easy way to raise exceptions , via the ``error``
procedure

 ``(error who message . args)``

and the ``syntax-violation`` procedure

 ``(syntax-violation who message form subform)``

which raises syntax errors at compile time.

The identifier ``who`` is usually the name of the function raising the error,
whereas the string ``message`` is the error message we want to display.
We can also pass exception arguments which are collected in the so-called
lists of irritants::

 > (error 'from-repl "An example error raised frome the REPL" 'arg1 'arg2)
 Unhandled exception
  Condition components:
    1. &error
    2. &who: from-repl
    3. &message: "An example error raised frome the REPL"
    4. &irritants: (arg1 arg2) 

Unfortunately, there is no shortcut for trapping such simple errors,
therefore
I will have to implement my own by using the R6RS ``guard``
macro, which is quite cumbersome to use (at least for me).

You can find all the gory details about the condition system and
the ``guard`` macro in the `R6RS library`_; the basic syntax is
``(guard (<variable>  <cond clause1> <cond clause2> ...) <body>)``
where ``<variable>`` is the name of the variable describing
the condition, the clauses are like in a ``cond`` expression
and the ``body`` is an expression which may raise conditions.

We can implement a simple ``_try-except``
expression on top of ``guard`` as follows:

$$try:_TRY-EXCEPT

``_try-except`` is able to trap exceptions coming from ``error`` or
from ``syntax-violation``: those a small subclass of all available
conditions, but they are enough for our purposes. In particular,
such exceptions are not hierarchical, so we are not trying to
catch a class of exceptions, but just a set of specific
exceptions identified by their names (the ``who`` attribute).
For instance, we could write a calculator of numeric expressions
as follows:

$$CALC

Here are a few tests:

$$TESTS

try .. except .. else
---------------------------------------------

Python ``try .. except`` statement also features an ``else`` clause which
is invoked if the exceptions is not raised. Our ``_try-except`` syntax
does not have an ``else`` clause, since it is unneeded. We can just
put the content of the ``else`` clause in the body of the expression:
if there is no exception, the code will be run, otherwise it will be not.
The only trick is that the expression must return the value of the
original expression, not the value of the ``else`` clause. That
can be easily accomplished by means of a ``begin0`` form.

``begin0`` is a macro which takes a number of expressions and returns
the zero-th expressions: it is somewhat the opposite of ``begin``, which
takes the last expression. ``begin0`` is not standard, however it is
built-in in various Scheme implementations (for instance in PLT Scheme
and Gauche) and has equivalent in various functional languages (for
instance ``before`` in SML).                         

Here is practical example of how you can convert a ``try ..  except .. else``
form into a ``_try-except`` form, by using ``begin0``::

 import ieee

 def save_invert(x):
     try:
        res = 1/x
     except ZeroDivisionError, e:
        print e
        return if x>0 ieee.PINF else ieee.MINF
     else:
        print 'ok'
        return res

becomes

$$SAFE-INVERT

Finalization in Scheme
---------------------------------------------------------

One work is not finished. We have just implemented a ``try .. except``
syntax, but if we really want to parallels Python, we need to
extend it ti a ``try .. except .. finally`` syntax.

Scheme does not have
a standard syntax for ``try .. finally``, however, it has a higher
order function called ``dynamic-wind`` which takes three thunks
(the *before* thunk, the *body* thunk, and the *after* thunk) and
execute them in order: in particular the after thunk is run
even if there is an exception or the control flow of the
body is non trivial (for instance if the body invokes a
continuation, but this is a subject we will leave for a
future episode). Therefore it is pretty obvious that
a ``try .. finally`` macro can be implemented in terms
of ``dynamic-wind`` as follows:

$$try:_TRY-FINALLY

Notice that we did not enforce ``finally`` to be a literal identifier
here, since ``_try-finally`` is intended to be a helper syntax
which is never called directly. The final ``try`` syntax will be
built on top of ``_try-finally`` and will enforce ``finally`` to
be a literal identifier.

Here is an example showing that ``_try-finally`` does its job, i.e.
the ``finally`` clause is honored even in presence of errors::

 > (_try-finally
    (error 'some-error "An example")
    (finally (display "done")))
 Unhandled exception
  Condition components:
    1. &error
    2. &who: some-error
    3. &message: "An example"
    4. &irritants: ()
 done>

At this point, it is easy to define a fully featured
``(try .. except .. else .. finally)`` construct:

$$try:TRY

The construct here is less powerful than the Python equivalent,
since exceptions are identified by a simple identifier and
not by classes, so that you cannot catch subclasses of
exceptions, but it is enough to be used in the following
episodes. 

.. _dynamic-wind: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_764
.. _R6RS library: http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-8.html


|#

(import (rnrs) (sweet-macros) (easy-test) (ikarus))

;BEGIN0
(def-syntax (begin0 e0 e ...)
  #'(let ((out e0)) e ... out))
;END

;CALC
(def-syntax (calc numeric-expr)
  #'(_try-except
     (let ((n numeric-expr))
       (if (number? n) n
           (error 'calc "not a number" n)))
     (except (arithmetic-error / * + -)
             (display arithmetic-error)
             (newline)
             (condition-message arithmetic-error))
     (except (other-error)
             (display other-error)
             (newline)
             "some error")))
;END


;;SAFE-INVERT
(define (safe-invert x)
  (assert (number? x))
  (_try-except
   (begin0 (/ 1 x)
     (display "ok\n"))
   (except (exn /)
     (display "zero division error\n")
     (if (> x 0) +inf.0 -inf.0))))
;END

;;_TRY-FINALLY
(def-syntax (_try-finally e e* ... (finally f f* ...))
  #'(dynamic-wind
        void
        (lambda () e e* ...)
        (lambda () f f* ...)))
;;END

;;TESTS
(run
 (test "0" (calc (+ 1 1)) 2)
 (test "1" (calc (/ 1 0)) "division by 0")
 (test "2" (calc (+ 1 "0")) "not a number")
 (test "3" (calc "a") "some error"))
;;END
