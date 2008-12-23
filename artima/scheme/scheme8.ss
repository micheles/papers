#|
Quoting
----------------------------------------------------------------

A distinguishing feature of the Lisp family of languages is the existence
of a quoting operator denoted with a quote ``'`` or with ``(quote )``,
the first form being syntacting sugar for the second.
The quoting operator works as follows:

1. on primitive values such as numbers, literal strings, symbols etc, it
   works as an identity operator:

::

 > '1
 1
 > '"hello"
 "hello"
 > ''a
 'a

2. expressions are converted into lists;
   for instance ``'(display "hello")`` is the list

::

 > (list 'display '"hello")
 (display "hello")

whereas ``'(let ((x 1)) (* 2 x))`` is the list

::

 > (list 'let (list (list 'x '1)) (list '* '2 'x))
 (let ((x 1)) (* 2 x))

et cetera.

Hence every Scheme/Lisp program admits a natural representation as
a (nested) list of symbols and primitive values: *code is data*. On
the other hand, every nested list of symbols and primitive values
corresponding to a syntactically valid Scheme/Lisp programs can be
executed, both at *runtime* - with ``eval`` - or at *compilation time*
- through macros. The consequences are fascinating: since every program
is a list, it is possible to write programs that, by building lists,
build other programs. Of course, you can do the same in other
languages: for instance in Python you can generate strings
corresponding to valid source code and you can evaluate such strings
with various mechanisms
(``eval``, ``exec``, ``__import__``, ``compile``, etc). In C/C++
you can generate a string, save it into a file and compile it to
a dynamic library, then you can import it at runtime;
you also have the mechanism of pre-processor macros at your disposal
for working at compile time. The point is that there is no language where
code generation is as convenient as in Scheme/Lisp where it is buil-in,
thanks to*s*-expressions or, you wish, thanks to parenthesis.

Quasi-quoting
-------------------------------------------------

In all scripting languages there is a form of *string interpolation*;
for instance in Python you can write

::

 def say_hello(user):
      return "hello, %(user)s" % locals()

In Scheme/Lisp, there is also a powerful form of *list interpolation*::

 > (define (say-hello user)
     `("hello" ,user))

 > (say-hello "Michele")
    ("hello" "Michele")

The *backquote* or ``(quasiquote )`` syntax ````` introduces a list to be
interpolated (*template*); it is possible to replace some
variables within the template, by prepending to them the *unquoting*
operator ``(unquote )`` or ``,``, denotated by a comma. In
our case we are unquoting the user name, ``,user``.  The function
``say-hello`` takes the user name as a string and returns a list
containing the string ``"hello"`` together with the username.

There is another operator like ``unquote``, called
``unquote-splice`` or comma-at and written ``,@``, which works as follows::

 > (let ((ls '(a b c))) `(func ,@ls))
 (func a b c)

In practice ``,@ls`` "unpack" the list ``ls`` into its components: without
the splice operator we would get::

 > (let ((ls '(a b c))) `(func ,ls))
 (func (a b c))

The power of quasiquotation stands in the code/data equivalence:
since Scheme/Lisp code is nothing else than a list, it is easy
to build code by interpolating a list template.
For instance, suppose we want to evaluate a Scheme expression
in a given context, where the contexts is given as a list of
binding, i.e. a list names/values::

 (eval-with-context '((a 1)(b 2) (c 3))
   '(* a  (+ b c)))

How can we define ``eval-with-context``? The answer is by ``eval``-uating
a template::

 (define (eval-with-context ctx expr)
  (eval `(let ,ctx ,expr) (environment '(rnrs))))

Notice that ``eval`` requires a second argument that specifies the language
known by the interpreter; in our case we declared that ``eval`` understands
all the procedures and macros of the most recent RnRS standard (i.e.
the R6RS standard). The environment specification has the same syntax
of an ``import``, since in practice it is the same concept: it is
possible to specify user-define modules as the ``eval`` environment.
This is especially useful if you have security concerns, since you
can run untrusted code in a stricter and safer subset of R6RS Scheme.

``eval`` is extremely powerful and sometimes it is the only possible
solution, in particular when you want to execute generic code at
runtime, i.e. when you are writing an interpreter. However, often
you only want to execute code known at compilation time: in this case
the job of ``eval`` can be done more elegantly by a macro. When that
happens, in practice you are writing a compiler.

Programs writing programs
------------------------------------------------

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/writer.jpg
 :class: right

Once you realize that code is nothing else than data, it becomes easy
to write programs taking in input source code and generating in output
source code, i.e. it is easy to write a compiler.
For instance, suppose we want to convert the following code fragment

::

   (begin
    (define n 3)
    (display "begin program\n")
    (for i from 1 to n (display i)); for is not defined in R6RS
    (display "\nend program\n"))  

which is not a valid R6RS program into the following program, which is
valid according to the R6RS standard::

   (begin
    (define n 3)
    (display "begin program\n")
    (let loop (( i 1)) ; for loop expanded into a named let
      (unless (>= i  n) (display i) (loop (add1 i))))
      (display "\nend program\n")))

``begin`` is the standard Scheme syntax to group multiple expressions
into a single expression *without introducing a new scope* (you may
introduce a new scope with ``let``) and *preserving the evaluation order*
(in most functional languages the evaluation order is unspecified).

More in general, we want to write a script which is able to convert

::

 (begin                                 (begin
   (expr1 ...)                            (expr1' ...)
   (expr2 ...)             -->            (expr2' ...)
    ...                                    ...
   (exprN ...))                           (exprN' ...))

where the expressions may be of kind ``for`` or any other kind not
containing a subexpression of kind ``for``.
Such a script can be thought of as a preprocessor
expanding source code from an high level language with a primitive
``for`` syntax into a low level language without a primitive
``for``. Preprocessors of this kind are actually very primitive
compilers, and Scheme syntax was basically invented to make the
writing of compilers easy.

In this case you can write a compiler expanding ``for`` expressions
into named lets as follows:

@@simple-compiler.ss

Running the script you will see that it replaces the ``for`` expression
with a *named let* indeed. It is not difficult to extend the compiler
to make it able to manage sub-expressions (the trick is to use
recursion) and structures more general than ``begin``: but I leave
that as an useful exercise. In a future episode I will talk of
*code-walkers* and I will discuss how to convert generic source code.
In general, one can convert s-expression based source code by using
an external compiler, as we did here, or by using the buil-in mechanism
provided by Scheme macros. Scheme macros are particularly powerful,
since they feature extremely advanced pattern matching capabilities:
the example I have just given, based on the primitive list operations
``car/cdr/map`` is absolutely primitive in comparison.

The next episode will be entirely devoted to macros. Don't miss it!

Appendix: solution of the exercises
------------------------------------------------------------

In the latest episode I asked you to write an equivalent (for
lists) of Python built-ins ``enumerate`` and ``zip``. Here I
give my solutions, so you may check them with yours.

Here the equivalent of Python ``enumerate``:

$$PY-ENUMERATE

and here is an example of usage::

 > (py-enumerate '(a b c))
   ((0 a) (1 b) (2 c))

Here is the equivalent of Python ``zip``:

$$ZIP

Here is an example of usage::

 > (zip '(0 a) '(1 b) '(2 c))
 ((0 1 2) (a b c))

Notice that ``zip`` works like the transposition operation
in a matrix: given the rows, it returns the columns of the matrix.
|#

(import (rnrs))

;PY-ENUMERATE
(define (py-enumerate lst)
  (let loop ((i 0) (ls lst) (acc '()))
    (if (null? ls) (reverse acc)
        (loop (+ 1 i) (cdr ls) (cons `(,i ,(car ls)) acc)))))
;END

(display (py-enumerate '(a b c)))

;ZIP
(define (zip . lists)
 (apply map list lists))
;END

(display (zip '(0 a) '(1 b) '(2 c)))
