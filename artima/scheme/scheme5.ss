#|
About tail call optimization (and the module system)
=================================================================

In order to speak of Scheme performance one needs to write at least a
few benchmarks. This is not completely trivial, for at least a couple
of reasons. The first reason is shallow, but it may be baffling for
beginners: since there is no ``for`` loop in Scheme, how are we
supposed to write a benchmark, which is usually based on running many
times the same instructions and measuring the total time spent? We
will see in a moment that there is an easy solution to this question
(use recursion). On the other hand, there is a second more serious
question: since there is no fully portable way to write a library in
Scheme, how can we write a benchmark library?  There is no real answer
to this question, so we will restrict ourselves to R6RS-compliant
Scheme implementations where there is a standard concept of library.

There are no ``for`` loops in Scheme
-------------------------------------------------------

The ``for`` loop is missing in Scheme as a primitive construct since
it is useless in a language that guarantees tail call optimization.
If you studied the concept of *tail call* at the college you know what
I am talking about; on the other hand, if you forgot it, or if you did
study Physics like myself, it is worth spenting a couple of words on
the subject. The ones who wants to know more, may consult
this `Wikipedia article`_.

.. image:: Ouroboros.png

The important point beyond the tail recursion concept
is that it is always possibile to convert a ``for``
into a recursive function in *tail call* form, i.e. a recursive
function returning a value or a call to itself. For instance,
the Python loop::

  # a loop printing 1 2 3
  for i in range(1,4):
     print i,

can be converted as a recursive function ``print_up_to_3``::

 def print_up_to_3(i):
    if i == 4: return
    print i, 
    return print_up_to_3(i+1)

 print_up_to_3(1)

Here the last instruction of the function (the tail) is a call to
itself, hence the name *tail call*.

The tail call optimization is an optimization guaranteed by the
Scheme language. Scheme compilers/interpreters are able
to recognize recursive functions in tail call form and to convert
them internally in ``for`` loops. As a consequence, the programmer
has no need to write ``for`` loops directly: she can just use
recursive function. Our example would look as follows in Scheme::

 (define (print-up-to-3 i)
    (unless (= i 4) 
      (display i) (display " ") 
      (print-up-to-3 (+ i 1))))

 (print-up-to-3 1)

This works, but it is not really readable; to improve the situation Scheme
provides a little syntactic sugar called *named let*::

  (let loop ((i 1))
    (unless (= i 4) 
       (display i) (display " ") 
       (loop (+ i 1))))

Traditionally the function in the *named let* construct is called ``loop``
to make clear to the programmer that we are emulating a ``for`` loop.
In this example ``loop`` is exactly equivalent to ``print-up-to-3``.

Let me point out two things before closing this paragraph:

1) there are other ``let`` forms, used to define local variables.
   The simplest one is ``let``::

     > (let ((a 1) (b 2)) (+ a b)) ; => 3

   The scope of ``a`` and ``b`` is limited to the current block;
   if ``a`` and ``b`` are defined outsid the ``let`` block, internally
   ``a`` and ``b`` *shadow* the outer names.

2) there is actually a ``do`` loop in the language, but it is cumbersome
   to use and redundant because the *named let* allows you to perform
   anything ``do`` does. I see it as an useless construct in a language
   that would like to be minimalist but it is not.

.. _Wikipedia article: http://en.wikipedia.org/wiki/Tail_call_optimization

There is no portable module system
---------------------------------------------------------------------

As I have anticipated before, libraries are the weak point of Scheme.
There are few libraries available and it is also difficult to
write portable libraries. The reason is that historically Scheme
never had any standard module system until very recently, with
the R6RS document: that means that nearly all current implementations
provide different and incompatible module systems.

In order to understand the reason for this serious lack, you must
understand the philosophy behind Scheme, i.e. the so called
`MIT approach`_: things must be done well, or not at all.
For thirty years the Scheme community has not been able to converge on 
a well done single module system. It is only in 2007 that a standard module
system has been blessed by the Scheme committee: but even that
was done was a lot of opposition and there are implementors who
said they will *never* support R6R5.

As a consequence of history and mentality, even as pof now, if you
want to write a library for implementation X, you need to do a lot of
boring and uninspiring work to port the library to implementations Y,
Z, W, ...  (there are *dozens* of different implementations).
Moreover, a few implementations do not have a module system at all, so
you may be forced to solve name clashes issue *by hand*, changing the
names of the functions exported by our library, if they shadow names
coming from third party libraries (!)

Personally, I picked up Scheme 5 years ago, but never used it
because of the lack of a standardized module system. The main reason why
I have decided to go back to Scheme and to write this series is the
coming of the R6RS document last year. The R5RS standard has lots of defects,
but at least now I can write a library and I can have people using
different implementations install it and use it (nearly) seemlessly.

.. _MIT approach: http://www.jwz.org/doc/worse-is-better.html
.. _psyntax: http://www.cs.indiana.edu/~aghuloum/r6rs-libraries/index.html
.. _not trivial at all: http://www.cs.utah.edu/plt/publications/macromod.pdf

Since there is some hope for a large diffusion of R6RS module system
in the future, I have decided to use it and to ignore implementations
not supporting it. I should notice however that there are
solutions to run R6RS modules on top of R5RS implementations, like
the psyntax_ package, but I have not tried it, so I cannot comment
on its reliability.

As first example of usage
of the R6RS module system, I will define a ``repeat`` library exporting
a ``call`` function which is able to call a procedure ``n`` times.
Here is the code, that should be self-explanatory::

 (library (repeat)
   (export call)
   (import (rnrs))
  
   (define (call n proc . args)
     (let loop ((i 0))
       (when (< i n) (apply proc args) (loop (+ 1 i))))))

The ``export`` declaration corresponds to Python's ``__all__``:
only the names listed in ``export`` are exported. In this case we
will export only the function ``(call n proc
. args)``. Notice the dot in the argument list: that means that the functions
accept a variable number of arguments, which are collected in the list
``args``.  In other words, ``. args`` is the moral equivalent of
``*args`` in Python, with some difference that we will ignore for
the moment. The ``apply`` function applies the argument list to the
input procedure ``proc``, which is called many times until the index
``i`` reaches the value ``n``.


``(import (rnrs))`` imports all the libraries of the current version of the 
"Revised Report on Scheme", i.e. the R6RS report. At the REPL this is
automatically done by the system, but for batch scripts it is mandatory
(as Pythonistas say *explicit is better than implicit*). It is also
possible to import subsections of the whole library. For instance
``(import (rnrs base))`` imports only the base library of the R6RS,
``(import (rnrs io))`` imports only the I/O libraries, et cetera.

The usage of the libray is trivial: it is enough to put the file
``repeat.sls`` somewhere in the Ikarus search path (specified
by the environment variable ``IKARUS_LIBRARY_PATH``). Then,
you can import the library as follows::

 $ rlwrap ikarus
 Ikarus Scheme version 0.0.2
 Copyright (c) 2006-2007 Abdulaziz Ghuloum
 > (import (repeat))
 > (call 3 display "hello!\n")
 hello!
 hello!
 hello!

By default ``(import (repeat))`` imports all the names exported by
the module ``repeat``, something that a Pythonista would never do
(it is equivalent to a ``import * from repeat``); fortunately it is
possible to list the names to be imported, or to add a custom prefix::

 > (import (only (repeat) call)); import only call from repeat
 call
 #<procedure call>
 > (import (prefix (repeat) repeat:)); import all with prefix repeat:
 > repeat:call
 #<procedure call>

A simple benchmark
-----------------------------------------------------------------

.. _episode 4: http://www.artima.com/weblogs/viewpost.jsp?thread=239568

The main advantage of Scheme with respect to Python is the performance.
In order to show the differences in performance I will go back to
the factorial example of `episode 4`_. I will compare the following
Python script::

 # fact.py
 import sys, timeit

 def fact(x):
     if x == 0: return 1
     else: return x * fact(x-1)

 if __name__ == '__main__':
     n = int(sys.argv[-1])
     t = timeit.Timer('fact(%d)' % n, 'from fact import fact')
     print t.repeat(1, number=10000000)
     print fact(n)

with the following R6RS-compliant script (written in Ikarus Scheme)::

 ; fact.ss
 (import (rnrs) (only (repeat) call) (only (ikarus) time))

 (define (fac x)
   (if (= x 0) 1
       (* x (fac (- x 1)))))

 (define n
   (string->number (car (reverse (command-line)))))

 (time (call 10000000 (lambda () (fac n))))
 (display "result:") (display (fac n)) (newline)


.. image:: clessidra.png
   :width: 175

I will notice two things:

1. Python manage to compute the factorial of 995, but then it faces
   the stack wall and it raises a
   ``RuntimeError: maximum recursion depth exceeded`` whereas Scheme
   has no issues whatsoever;

2. In order to compute the factorial of 995 ten thousands times, Python 
   takes 15.2 seconds, whereas Ikarus takes 7.2 seconds.

Notice that since the factorial of 995 is a *large* number, the computation
time is spent in multiplication of large numbers, which are implemented
in C. Python has its own implementation of
long integers, whereas Ikarus uses the GNU Multiprecision library (gmp_):
the times measured here mean that the gmp_ implementation of
long integers is more efficient than the Python one, but they
say nothing on the relative performances of the two languages.
It is more interesting to see what happens for small numbers.
For instance, in order to compute the factorial of 7 for 10 millions
of times, Python takes 30.5 seconds, whereas Ikarus taks
3.08 seconds and thus it is nearly *ten times* faster than Python.
This is not surprising at all, since function calls in Python
are especially slow whereas they are optimized in Scheme. Moreover
Ikarus is a native code compiler.

That means that Ikarus' REPL_ works by compiling expressions to native code,
whereas Python's REPL compiles to bytecode. The technology is called
incremental compilation and it is commonly used in Lisp languages
from decades, even it may look futuristic for C/C++ programmers.
The factorial example is not very practical (on purpose), but it
is significant, in the sense that it is legitimate to expect
good performances from Scheme compilers. The fastest
Scheme compiler out there is Stalin_, but I would not recommend
it to beginners.

.. _REPL: http://en.wikipedia.org/wiki/REPL
.. _gmp: http://gmplib.org/
.. _Stalin: http://community.schemewiki.org/?Stalin

The next episodes will be devoted to the dangers of benchmarks,
do not miss it!
|#