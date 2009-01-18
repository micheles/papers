#|
The problem of multiple evaluation
-------------------------------------------------------------------

In `episode #10`_ I gave an example of a macro implementing
a C-like ``for`` loop and I said that it was suffering from the
problem of multiple evaluation. Here I explain what the
problem is and how to cure it. In order to understand the issue,
you must always remember that macros *expand* code at compile time,
but they not *evaluate* it: that means that pattern variables do *not*
correspond to evalued expression, as ordinary variables, but they
correspond to expressions to be evaluated later, at runtime.

As a consequence, it is easy to write macros
that evaluate expressions more times than needed. For instance,
consider the following simplified version of a C-like ``for`` loop,
with a runtime type check::

 (def-syntax (for i start end body ...)
   #'(begin
      (assert (and (number? start) (number? end))); type-check
      (let loop ((i start))
        (unless (>= i end) body ... (loop (+ i 1))))))

Suppose the variable ``end`` to be determined dynamically
with a computation::

 > (define (get-end)
   (printf "computing the value of end\n")
  3)

Then our naive macro suffers for the multiple evaluation problem::

 > (for i 0 (get-end) 'do-nothing)
 computing the value of end
 computing the value of end
 computing the value of end
 computing the value of end
 computing the value of end

As you see, in this example ``end`` is recomputed 5 times!
The reason is clear if you look at the expansion of the macro::

 > (syntax-expand (for i 0 (get-end) 'do-nothing))
 (begin
  (assert (and (number? 0) (number? (get-end))))
  (let loop ((i 0))
    (unless (>= i (get-end)) 'do-nothing (loop (+ i 1)))))

The ``get-end`` function is called once in the assertion and
four times in the loop; that is inefficient and can have very
dramatic effects if the function has side effects.
The solution is to save the value of ``end`` (and we could do the same
for the value of ``start``, which is computed twice) in a variable::

 (def-syntax (for i start end body ...)
    #'(let ((s start) (e end))
      (assert (and (number? s) (number? e)))
      (let loop ((i s))
        (unless (>= i e) body ... (loop (+ i 1))))))

Now ``get-end`` is called only once and we are all happy :-)

As an exercise, you could extend ``for`` to accept a generic step.
You can find the solution in the Italian `original version`_ of this article,
which is quite different and uses ``syntax-rules``.

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/gears.gif
   :width: 240

.. _episode #10: http://www.artima.com/weblogs/viewpost.jsp?thread=240805
.. _original version: http://stacktrace.it/2008/04/le-avventure-di-un-pythonista-schemeland8/

Taking advantage of multiple evaluation
-------------------------------------------------------------

Sometimes we can make good use of the multiple evaluation "feature".
For instance, let me consided again the higher order
function ``call`` I introduced in `episode #5`_, when
discussing benchmark. That function has an issue: it is
called at each iteration in the inner loop and therefore it wastes
time. However, it is possible to replace the higher order function
with a macro, therefore avoiding the cost of a function call.
Here is the code for a ``repeat`` macro doing the job of ``call``:

$$repeat-macro:

``repeat`` expands into a loop and therefore the body is evaluated ``n``
times, which is exactly what we need for a benchmark.
To check that the macro is effectively more efficient, I did measure
the time spent in summing 1+1 ten million of times:

$$repeat-benchmark:

I took the number ``n`` from the command line arguments
in order to fool the compiler: if I hard coded ``(+ 1 1)``, the compiler
would replace it with 2 at compilation time, therefore not performing
the computation! (in the original version of this episode I made that
mistake, thanks to Aziz Ghuloum for pointing it out).
The output of the script is the following::

 $ scheme-script repeat-benchmark.ss 1
 running stats for (call 10000000 + 1 n):
     no collections
     396 ms elapsed cpu time, including 0 ms collecting
     394 ms elapsed real time, including 0 ms collecting
     32 bytes allocated
 running stats for (repeat 10000000 (+ 1 n)):
     no collections
     40 ms elapsed cpu time, including 0 ms collecting
     40 ms elapsed real time, including 0 ms collecting
     0 bytes allocated

As you see, avoiding the function call makes a lot of difference
(the benchmark is 10 times faster!) since the great majority of the
time is wasted in calling the benchmarking
function and not in the real addition.
Here the improvement is spectacular since summing two integers is a
very fast operation: replacing ``call`` with ``repeat`` in the
benchmark factorial does not make a big difference instead.

.. _episode #5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

A micro-framework for unit tests
-----------------------------------------------------------------

It is time to give a more practical example of Scheme macros.  In this
paragraph, I will define a very simple unit test framework called
``easy-test``.

Clearly, there are already unit test frameworks
available for Scheme, including two SRFIs (64_ and 78_); my interests
here is not in the testing framework, it is in the implementation, which makes
a pedagogical exercise in macrology.

The source code takes just a page:

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/feu_rouge.jpg

$$easy-test:

The core of the framework is the ``test`` macro, which is a bit different
from the macros we have defined until now. The reason why the ``test``
macro is different is that it expands into a ``lambda``-expression
and therefore the arguments
of the macro are evaluated only when the ``lambda`` function is called
and not a definition time. In other words, we are using a pattern of
*delayed evaluation* here. This is important, since we want to distinguish
the definition of a test from its execution. For instance, let me
define a trivial test::

 > (import (easy-test))
 > (define test1 (test "1+1=2" (+ 1 1) 2))

The first argument of the macro is a string describing the test, which
is nice to have in the error message for failed tests; the second
argument of the macro is the expression to check and the third
argument is the expected result.

Macro application results in a
function which is able to respond to the commands
``'descr`` (returning the description string), ``'values``
(returning a list with the quoted input expression and the quoted
expected output) and ``'run`` (returning the result of the test, as
a boolean flag). This is implemented via the `case expression`_ in
the ``test`` macro::

 (case cmd
   ((descr) description)
   ((values) '(expr  expected))
   ((run) (equal? expr expected))
   (else (error 'test "Invalid command" cmd)))

Here is how it works in our example::

 > (test1 'descr)
 "1+1=2"
 > (test1 'values)
 ((+ 1 1) 2)
 > (test1 'run) ; the test passed
 #t

The framework provides
three predefined functions ``print-nothing``,
``print-msg`` and ``print-dot`` to print feedback about how the
tests are going; moreover, it is possible to define custom
reporting functions. A reporting function is simply a function
with three arguments ``(descr expr expected)`` where ``descr``
is a string with the description of the test,
``expr`` is the expression to be checked and ``expected`` is the expected
result. You can specify the reporting functions to use by defining
a test runner as in this example::

 > (define run-quiet (runner print-nothing print-msg))
 > (run-quiet
     (test "1+1=2" (+ 1 1) 2)
     (test "2*1=2" (* 2 1) 2)
     (test "2+2=3" (+ 2 2) 3))
 '2+2=3' failed. Expected 3, got 4
 (2 1)

The runner returns a list with the number of passed tests
and failed tests (in our case ``'(2 1)``).

It is also possible to use the default runner (``run``):
the framework will use the default reporting functions,
i.e. ``print-dot`` for successful tests and ``print-msg`` for failed
tests.

.. _64: http://srfi.schemers.org/srfi-64/srfi-64.html
.. _78: http://srfi.schemers.org/srfi-78/srfi-78.html
.. _case expression: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_384
|#
(import (rnrs) (easy-test))

(define succ-fail
  (run 
   (test "1+1=2" (+ 1 1) 2)
   (test "2*1=2" (* 2 1) 2)
   (test "2+2=3" (+ 2 2) 3)))
