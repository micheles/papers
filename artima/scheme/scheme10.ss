#|
A couple of common beginner's mistakes
-----------------------------------------------------------------

If you try to write macros of your own, you will likely incur in
some beginner's mistake, so I think it is worth warning my readers
about a couple of common errors.

The first one is forgetting the ``begin``
expression in the line ``(begin (define name value) ...)``::

 > (def-syntax (multi-define-wrong (name ...) (value ...))
    #'((define name value) ...))

If you try to use this macro, you will get an exception::

 > (multi-define-wrong (a) (1))
 Unhandled exception
 Condition components:
   1. &who: define
   2. &message: "a definition was found where an expression was expected"
   3. &syntax:
       form: (define a 1)
       subform: #f

The problem is that Scheme interpreter interprets a pattern of the form
``(func arg ...)`` as a function application, but in this case
``func`` is the definition ``(define a 1)`` which is certainly not an
function, it is not even an expression!

Actually, R6RS Scheme distinguishes
definitions from expressions, a little bit like in other
languages statements are distinguished from expressions, except that
in Scheme there are no statements other than definitions.
You will get exactly the same error if you try to print a definition
``(display (define a 1))``: since a definition does not return anything,
you cannot print it.

A second common mistake is to forget the sharp-quote
(``#'``) syntax in the skeleton or in the guard.
If you forget it - for instance if
you write ``(begin (define name value) ...)`` instead of ``#'(begin
(define name value) ...)`` - you will get a strange error message
``reference to pattern variable outside a syntax form``. To understand
the message, you must understand what a *syntax form* is. That
requires a rather detailed explanation that I will live for a future
episode. For the moment, be content with a simplified explanation. A
syntax form is a special type of quoted form: just as you write
``'(some expression)`` or ``(quote (some expression))`` to keep
unevalued a block of (valid or invalid) Scheme code, you can write
``#'(some expression)`` or ``(syntax (some expression))`` to denote a
block of (valid or invalid) Scheme code which is used in a macro and
contains pattern variables. Pattern variables must always be written
inside a ``syntax`` expression, so that they can be replaced with
their right values when the macro is expanded at compile time.

Note: R6RS Scheme requires the syntax ``#'x`` to be interpreted as
a shortcut for ``(syntax x)``; however there are R5RS implementation
(such as Chicken Scheme) that do not allow the ``#'x`` syntax
(actually Chicken allows the syntax ``#'x``, but
with a different meaning). If you want to be fully portable
you should use the extended form ``(syntax x)``. However, all the
code in this series is intended to work on R6RS Schemes, therefore
I will always use the shortcut notation ``#'`` which is way more
readable.

Guarded patterns
----------------------------------------------------------------

There a few things I did not explain fully when introducing the
``multi-define`` macro.  For instance, what happens if the number of
the identifiers does not match the number of the values?  Of course,
you get an error::

 > (multi-define (a b c) (1 2))
 Unhandled exception
  Condition components:
    1. &assertion
    2. &who: ...
    3. &message: "length mismatch"
    4. &irritants: ((#<syntax 1> #<syntax 2>) (#<syntax a> #<syntax b> #<syntax c>))

The problem is that the error message is a bit scary, with all those
``#<syntax >`` things. How do we get an error message which is less
scary to the newbie? Answer: by using the guarded patterns feature of
``sweet-macros``!

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/carabinieri.png

Here is an example:

$$MULTI-DEFINE

The line ``(= (length #'(name ...)) (length #'(value ...)))`` is
the guard of the pattern ``(multi-define (name ...) (value ...))``.
It checks *at compile time* that the number of names
matches the number of values; if the check is satified then
the macro is expanded correctly, otherwise a ``syntax-violation``
is raised (i.e. a *compile time* exception) with a nice
error message.

::

 > (multi-define (a b c) (1 2))
 Unhandled exception:
  Condition components:
    1. &who: multi-define
    2. &message: "The number of names and values does not mismatch"
    3. &syntax:
        form: ((a b c) (1 2))
        subform: #f

Because of their working at compile time,
guarded patterns are an ideal tool to check the consistency
of our macros (remember: it is very important to check for
errors as early as possible, and the earliest possible time is compile
time).

Literal identifiers
-------------------------------------------------------

Guarded patterns can also be (ab)used to recognize keyword-like
identifiers in a macro.  For instance, here is how you could implement
the semantics of the ``for`` loop discussed in `episode #8`_ with a macro
(notice how all the funny characters ``',@``` disappeared):

$$FOR

Here the R6RS primitive ``syntax->datum`` is used to convert the
syntax objects (we will say more about syntax objects in the future)
``#'from`` and ``#'to`` into regular Scheme objects
so that they can be compared for equality
with the literal identifiers ``'from`` and ``'to``.

You can check that the macro works by trying to use a wrong syntax.
For install if you mispell ``from`` as ``fro`` you will get a syntax
error *at compilation time*::

 > (for i fro 1 to 5 (display i))
 Unhandled exception:
  Condition components:
    1. &message: "invalid syntax"
    2. &syntax:
        form: (for i fro 1 to 5 (display i))
        subform: #f

Still, I say that this is an abuse of
guarded patterns, since ``syntax-match`` provides a builtin
mechanism just for this purpose. Moreover this macro is
subject to the multiple evaluation problem which will be discussed
in the next episode: thus I do not recommend it as an example
of good style when writing macros. Still, I have written it here
to compare it with our approach in `episode #8`_:
with this macro we have been able to
extend the Scheme compiler for within, with just a few lines of
code: that is much simpler than writing an external compiler
as a preprocessor, as we planned to do before.

Moreover, using pattern matching is much more readable than using
quasiquotation and the splice syntax, especially if you take full
advantage of the features of ``syntax-match``.  In particular, even if
I did not mention it before, ``syntax-match`` allows for literal
identifiers to be recognized in the patterns as keywords. This is what
the empty parenthesis are for. If you write ``(syntax-match (lit ...)
clause ...)`` the identifiers listed in ``(lit ...)`` will be treated
as literal identifiers in the macro scope. Literal identifiers can be
used to enhance readability, or to define complex macros.  For
instance our ``for`` macro can be written without need for guarded
patterns as::

 (def-syntax for
   (syntax-match (from to)
     (sub (for i from i0 to i1 action ...)
        #'(let loop ((i i0))
            (unless (>= i i1) action ... (loop (+ i 1)))))))
  
You can even introspect the literal identifiers recognized by ``syntax-match``::

 > (for <literals>)
 (from to)

Let me close this paragraph by suggesting an exercise in macrology.
Try to implement a Python-like ``for`` loop working as in the
following examples::


 > (for x in '(1 2 3)
     (display x))
 123
 > (for (x y) in '((a b) (A B) (1 2))
     (display x) (display y))
 abAB12

Clearly it should work for a generic number of arguments and ``in``
should be treated as a literal identifier. I will give the solution
in episode 12, so you will have some time to play. Have fun!

.. _episode #8: http://www.artima.com/weblogs/viewpost.jsp?thread=240793

Macros as performance hacks
-------------------------------------------------------------

Apart for their relevance as syntactic sugar (the examples of ``multi-define``
and ``for`` speak clearly), macros are also very
relevant for performance reasons. The point is that macros are
instructions for the compiler and as such they are expanded 
*at compilation time*. Therefore in some cases it is possible to
avoid expensive computations at runtime.
That may have enourmous impact on the performance of an
application.

.. image:: http://www.phyast.pitt.edu/~micheles/scheme//salvador-dali-clock.jpg

For instance, let me consided again the higher order
function ``call`` I introduced in `episode #5`_ when
discussing benchmark. That function has an issue: it is
called at each iteration in the inner loop and therefore it wastes
time. However, it is possible to replace the higher order function
with a macro, therefore avoiding the cost of a function call.
Here is the code for a ``repeat`` macro doing the job of ``call``:

@@repeat-macro.sls

To check that the macro is effectively more efficient, I did measure
the time spent in summing 1+1 ten million of times::
 
 $ rlwrap ikarus 
 Ikarus Scheme version 0.0.3+ (revision 1622, build 2008-10-11)
 Copyright (c) 2006-2008 Abdulaziz Ghuloum

 > (import (repeat))
 (time (call 10000000 + 1 1))
 running stats for (call 10000000 + 1 1):
    no collections
    444 ms elapsed cpu time, including 0 ms collecting
    442 ms elapsed real time, including 0 ms collecting
    32 bytes allocated

 > (import (repeat-macro))
 > (repeat 10000000 (+ 1 1))
 running stats for (repeat 10000000 (+ 1 1)):
    no collections
    64 ms elapsed cpu time, including 0 ms collecting
    64 ms elapsed real time, including 0 ms collecting
    0 bytes allocated

As you see, avoiding the function call makes a lot of difference
(the benchmark is 7 times faster!) since the great majority of the
time was wasted in calling the benchmarking
function and not the real addition.

Here the improvement is spectacular since summing two integers is a
very fast operation: replacing ``call`` with ``repeat`` in the
benchmark factorial does not make a big difference.

.. _episode #5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

|#

(import (rnrs) (ikarus) (sweet-macros) (repeat-macro) (easy-test))
 
(define (call n proc . args)
  (let loop ((i 0))
    (when (< i n) (apply proc args) (loop (+ 1 i)))))

(time (call 10000000 + 1 1))
(time (repeat 10000000 (+ 1 1)))

;FOR
(def-syntax (for i from i0 to i1 action ...)
  #'(let loop ((i i0))
      (unless (>= i i1) action ... (loop (+ i 1))))
 (and (eq? (syntax->datum #'from) 'from) (eq? (syntax->datum #'to) 'to)))
;END

(for i from 1 to 5 (display i))
(display (syntax-expand (for i from 1 to 5 (display i))))

;MULTI-DEFINE
  (def-syntax (multi-define (name ...) (value ...))  ; the pattern
    #'(begin (define name value) ...)                ; the skeleton
    (= (length #'(name ...)) (length #'(value ...))) ; the guard
     (syntax-violation 'multi-define 
      "Names and values do not mismatch" 
      #'((name ...) (value ...))))
;END

;(multi-define (a b) (1 2 3))

 (def-syntax for2
   (syntax-match (from to)
     (sub (for2 i from i0 to i1 action ...)
        #'(let loop ((i i0))
            (unless (>= i i1) action ... (loop (+ i 1)))))))

(for2 i from 1 to 5 (display i))

(test
 ("for2 patterns" (for2 <patterns>)
  => '((for2 i from i0 to i1 action ...)))
 
 ("for2 literals" (for2 <literals>)
  => '(from to))
 )

(display (syntax-expand (for2 i from 1 to 5 (display i))))
