#|
``syntax-match`` and introspection features of sweet-macros
-----------------------------------------------------------------

In the last episode I have defined a very simple ``multi-define`` macro
by using my own ``sweet-macros`` framework. I have also claimed
that sweet macros provides introspection facilities, but I have not shown
them. Here I will substain my claim.

For of all, let me show how you can get
the patterns accepted by ``multi-define``::

 > (multi-define <patterns>)
 ((multi-define (name ...) (value ...)))

Since ``multi-define`` is a simple macro it accepts only a single pattern.
However, it is possible to define macros with multiple patterns by relying
on the second form of ``def-syntax``, i.e.

 ``(def-syntax name transformer)``

where the transformer is a procedure which is typically built on
top of ``syntax-match``. For instance, suppose we wanted to extend
``multi-define`` to work also as a replacement of ``define``, i.e.
suppose we want to accept the pattern ``(multi-define name value)``
where ``name`` is an identifier. Here is how to do that
by using ``syntax-match``:

$$MULTI-DEFINE2

``syntax-match`` recognizes the literal identifier ``sub`` as an expected
keyword when it appears in the right position, i.e. at the beginning
of each clause. ``sub`` is there for two reasons:

1. in my opinion it makes the code more readable: you should read a clause
   ``(sub pattern skeleton)`` as "substitute a chunk of code matching the
   pattern with the code obtained by expanding the pattern variables inside
   the skeleton";

2. it makes ``syntax-match`` look different from ``syntax-case`` and
   ``syntax-rules``, which is fine, since ``syntax-match`` *is* a little
   different from the Scheme standard macro systems.

The identifier ``ctx`` that you see as first element of each pattern
denotes the context of the macro, a
concept that I will explain in a future installment; you can use any
valid identitier for the context, including the name of the macro
itself - that is a common convention.  If you are not interested in
the context (which is the usual case) you can discard it and use the
special identifier ``_`` to make clear your intent.

I leave as an
exercise to check that if you invert the order of the clauses the
macro does not work: you must remember to put the most specific clause
*first*.

In general you can get the source code for all the macros defined via
``def-syntax`` and ``syntax-match``. For instance, the source code (of
the transformer) of our original ``multi-define`` macro is the
following::

 > (multi-define <source>)
 (syntax-match ()
   (sub (multi-define (name ...) (value ...))
     #'(begin (define name value) ...)))

As you see, for better readability ``def-syntax`` use the name
of the macro for the context, but any name would do.

I have not explained everything there is to know about
``syntax-match``, but we need to leave something out for
the next episode, right?

A couple of common mistakes
-----------------------------------------------------------------

If you try to write macros of your own, you will likely incur in
mistakes. I think it is worth warning my readers
about a couple of such common mistakes.

The first one is forgetting the ``begin`` for macros expanding to
multiple expressions. For instance, you could be tempted to write
``multi-define`` as follows::

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

The problem is that Scheme interprets a pattern of the form
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

A second common mistake is to forget the sharp-quote ``#'``.
If you forget it - for instance if
you write ``(begin (define name value) ...)`` instead of ``#'(begin
(define name value) ...)`` - you will get a strange error message:
*reference to pattern variable outside a syntax form*. To understand
the message, you must understand what a *syntax form* is. That
requires a rather detailed explanation that I will leave for a future
episode.

For the moment, be content with a simplified explanation. A syntax
form is a special type of quoted form: just as you write ``'(some
expression)`` or ``(quote (some expression))`` to keep unevaluated a
block of (valid or invalid) Scheme code, you can write ``#'(some
expression)`` or ``(syntax (some expression))`` to denote a block
of (valid or invalid) Scheme code which is intended to be used in a
macro and contains pattern variables. Pattern variables must always be
written inside a ``syntax`` expression, so that they can be replaced
with their right values when the macro is expanded at compile time.

Note: R6RS Scheme requires the syntax ``#'x`` to be interpreted as
a shortcut for ``(syntax x)``; however there are R5RS implementation
(such as Chicken Scheme) that do not allow the ``#'x`` syntax
(actually Chicken allows the syntax ``#'x``, but
with a different meaning). If you want to be fully portable
you should use the extended form ``(syntax x)``. However, all the
code in this series is intended to work on R6RS Schemes, therefore
I will always use the shortcut notation ``#'`` which in my opinion
is *ways* more readable.

Guarded patterns
----------------------------------------------------------------

There are a few things I did not explain when introducing the
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
The macro will expand the patterns in the guard into lists
*at compile time*, then it will check that the number of names
matches the number of values; if the check is satified then
the skeleton is expanded, otherwise a ``syntax-violation``
is raised (i.e. a *compile time* exception) with a nice
error message::

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
syntax objects ``#'from`` and ``#'to`` into regular Scheme objects
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
guarded patterns, since ``syntax-match`` provides a built-in
mechanism just for that purpose. Moreover this macro is
subject to the multiple evaluation problem which I will discuss
in the next episode: thus I do not recommend it as an example
of good style when writing macros. Still, I have written it here
to compare it with our approach in `episode #8`_:
with this macro we have been able to
extend the Scheme compiler for within, with just a few lines of
code: that is much simpler than writing an external compiler
as a preprocessor, as we planned to do before.

``syntax-match`` has the built-in capability of recognizing literal
identifiers in the patterns as if they were keywords. This is what
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

;MULTI-DEFINE2
(def-syntax multi-define2
  (syntax-match ()
   (sub (ctx (name ...) (value ...))
     #'(begin (define name value) ...))
   (sub (ctx name value)
     #'(define name value))
  ))
;END
