#|
In this episode we will explain the meaning of the *code is data*
concept. To this aim we will discuss the  *quoting* operation which
allows to convert a code fragment into a list of symbols and primitive
values (i.e. converts code into data) and the *eval* operation which
allows to execute any list of symbols and primitive values corresponding
to valid code (i.e. converts data into code).

Quoting
----------------------------------------------------------------

A distinguishing feature of Lisp and derived languages is the existence
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

2. on composite objects (expressions)
   the quoting operator converts a code fragment into a list:
   for instance ``'(display "hello")`` denotes the list

::

 > (list 'display '"hello")
 (display "hello")

whereas ``'(let ((x 1)) (* 2 x))`` denotes the list

::

 > (list 'let (list (list 'x '1)) (list '* '2 'x))
 (let ((x 1)) (* 2 x))

et cetera.

Every Scheme/Lisp programs admits a natural representation as
a (nested) list of symbols and primitive values: *code is data*. On
the other hand, every nested list of symbols and primitive values
corresponding to a syntactically valid Schem/Lisp programs can be
evalued, both at *runtime* with ``eval`` or at *compilation time*
through macros. The consequences are fascinating: since every program
is a list, it is possible to write programs that, by building lists,
build other programs. Of course, you can do the same in other
languages: for instance in Python you can generate strings
corresponding to valid source code and you can evaluate such strings
with various mechanisms
(``eval``, ``exec``, ``__import__``, ``compile``, etc). In C/C++
you can generate a string, save it into a file and compile it to
a dynamic library, then you can import it at runtime; moreover,
you have the mechanism of pre-processor macros at your disposal
for simpler things. The point is that there is no language where
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

The *backquote* or *quasiquote* syntax ````` introduces a list to be
interpolated (*template*) where it is possible to replace some
variables with the *unquoting* operation, denotated by a *comma*. In
our case we are unquoting the user name, ``,user``.  The function
``say-hello`` takes the user name as a string and returns a list
containing the string ``"hello"`` together with the username.

There is another operator like ``unquote``, called
``unquote-splice`` or *comma-at* and written ``,@``, which works as follows::

 > (let ((ls '(a b c))) `(func ,@ls))
 (func a b c)

In practice ``,@ls`` "unpack" the list into its components: without
the splice operator we would get::

 > (let ((ls '(a b c))) `(func ,ls))
 (func (a b c))

The power of quasiquotation stands in the code/data equivalence:
since Scheme/Lisp code is nothing else than a list, it is easy
to execute code build by interpolating a list template.
For instance, suppose we want to evaluate a Scheme expression
in a given context, where the contexts is given as a list of
binding, i.e. a list names/values::

 (eval-with-context '((a 1)(b 2) (c 3))
   '(* a  (+ b c)))

How can we define ``eval-with-context``? The answer is ``eval``-uating
a template::

 (define (eval-with-context ctx expr)
  (eval `(let ,ctx ,expr) (environment '(rnrs))))

Notice that ``eval`` requires a second argument that specifies the language
known by the interpreter; in our case we declared that ``eval`` understands
all the procedures and macros of the most recent RnRS standard (i.e.
the R6RS standard). The environment specification has the same syntax
of an ``import``, since in practice it is the same concept: it is
possible to import in the ``eval`` environment user-define modules.

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
source code, i.e. it is easy to write an interpreter or a compile. The
difference is that in the first case the sourse code is converted and
executed (interpret) expression by expression, whereas in the second
case the code is converted (compiled) in blocks.
For instance, suppose we want to convert the program

::

   (begin
    (define n 3)
    (display "begin program\n")
    (for i from 1 to n (display i))
    (display "\nend program\n"))  

into the program

::

   (begin
    (define n 3)
    (display "begin program\n")
    (let loop (( i 1))
      (unless (>= i  n) (display i) (loop (add1 i))))
      (display "\nend program\n")))

More in general, we want to convert

::

 (begin                                 (begin
   (expr1 ...)                            (expr1' ...)
   (expr2 ...)             -->            (expr2' ...)
    ...                                    ...
   (exprN ...))                           (exprN' ...))

where the expressions may be of kind ``for`` or any other kind not
containing a subexpression of kind ``for``.
``begin`` is the standard Scheme syntax to group multiple expressions
into a single expression *without introducing a new scope* (you may
introduce a new scope with ``let``) and *preserving the evaluation order*
(in most functional languages the evaluation order is unspecified).

You can write such a compiler as follows::

 (import (rnrs) (only (ikarus) pretty-print))

 ;; a very low-level approach
 (define (convert-for-into-loop begin-list)
   (assert (eq? 'begin (car begin-list)))
   `(begin
      ,@(map (lambda (expr)
               (if (eq? 'for (car expr)) (apply convert-for (cdr expr)) expr))
             (cdr begin-list))))

 (define (convert-for i from i0 to i1 . actions)
   ;; from must be 'from and to must be 'to
   (assert (and (eq? 'from from) (eq? 'to to)))
   `(let loop ((i ,i0))
      (unless (>= i i1) ,@actions (loop (+ i 1)))))

 (pretty-print
  (convert-for-into-loop
   '(begin
      (define n 3)
      (display "begin program\n")
      (for i from 1 to n (display i))
      (display "\nend program\n")))) 

Running the script you will see that it replaces the ``for`` expression
with a *named let* indeed. It is not difficult to extend the compiler
to make it able to manage sub-expressions (the trick is to use
recursion) and structures more general than ``begin``: but I leave
that as an useful exercise. In future episode we will talk of
*code-walkers* and we will discuss how to convert generic source code.
In general ``convert-for-into-loop`` can be thought of as a preprocessor
expanding source code from an high level language with a primitive
``for`` syntax into a low level language without a primitive
``for``. Preprocessors of this kind (which are actually very primitive
compilers) can be implemented externally
to the language, but also internally, by using the buil-in mechanism
provided by Scheme macros. Scheme macros are particularly powerful,
since they feature extremely powerful pattern matching capabilities:
the example I have just given, based on the primitive list operations
``car/cdr/map`` is absolutely primitive in comparison.

The next episode will be devoted to macros entirely. Don't miss it!

Appendix: solution of the exercises
------------------------------------------------------------

In the last episode I have asked you to write an equivalent (for
lists) of Python built-ins ``enumerate`` and ``zip`` in Scheme. Here I
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
 (apply map (lambda x x) lists))
;END

(display (zip '(0 a) '(1 b) '(2 c)))
