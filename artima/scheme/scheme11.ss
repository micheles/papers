#|

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/gears.gif
 :width: 240

The problem of multiple evaluation
-------------------------------------------------------------------

In the previous episode I have given an example of a macro implementing
a C-like ``for`` loop and I have said that it was suffering from the
problem of multiple evaluation. In this section I explain what the
problem is and how to cure it. In order to understand the issue,
you must always remember that macros *expand* code at compile time,
but they not *evaluate* it: that means that pattern variables do *not*
correspond to evalued expression, as ordinary variables, but they
correspond to expressions to be evaluated later, at runtime.

As a consequence, it is easy write macros
that evaluate expression more times than needed. For instance,
consider the following simplified version of C-like ``for`` loop,
with a runtime type check::

 (def-syntax (for i start end body ...)
   #'(begin
      (assert (and (number? start) (number? end)))
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

As you see, ``end`` is recomputed 5 times!
The reason is clear if you look at the expansion of the macro::

 > (syntax-expand (for i 0 (get-end) 'do-nothing))
 (begin
  (assert (and (number? 0) (number? (get-end))))
  (let loop ((i 0))
    (unless (>= i (get-end)) 'do-nothing (loop (+ i 1)))))

The ``get-end`` function is called one in the assertion and
four times in the loop; it is clear that this fact can have
dramatic effects (if the function has side effects) apart from
being very inefficient.
The solution is to save the value of ``end`` (and we could do the same
for the value of ``start``, which is computed twice) in a variable:

 (def-syntax (for i start end body ...)
    #'(let ((s start) (e end))
      (assert (and (number? s) (number? e)))
      (let loop ((i s))
        (unless (>= i e) body ... (loop (+ i 1))))))

Now ``get-end`` is called only once and we are all happy :-)

I leave for exercise playing with the ``for``, for instance
by extending it to accept a generic step. You can find other
examples in the Italian `original version`_ of this article,
which is quite different and uses ``syntax-rules``.

.. _original version: http://stacktrace.it/2008/04/le-avventure-di-un-pythonista-schemeland8/

A micro-framework for unit tests
-----------------------------------------------------------------

It is time to give a more practical example of Scheme macros.  In this
paragraph, I will define a very simple unit test framework called
*easy-test*.  The source code takes just a page:

.. include-code:: easy-test.sls

Notice the line
``(let ((expr (begin e1 e2 ...)) (expected expect))``: it is
there to make sure that the input expression (``expr``) and the output
expression (``expected``) are evalued only once.
The usage of the framework is trivial::

 > (import (easy-test))
 > (test (success print-nothing) (failure print-msg)
      ("test 1+1=2" (+ 1 1) => 2)
      ("test 2*1=2" (* 2 1) => 2)
      ("test 2+2=3" (+ 2 2) => 3))
 'test 2+2=3' failed. Expected 3, got 4
 (2 1)

The ``test`` macro returns a list with the number of passed tests
and failed tests (in our case ``'(2 1)``). The framework provides
three predefined functions ``print-nothing``,
``print-msg`` and ``print-dot`` to print feedback about how the
tests are going; moreover, it is possible to define custom
reporting functions. A reporting function is simply a function
with three arguments ``(descr
expr expected)`` where ``descr`` is a string with the description of the test,
``expr`` is the expression to be checked and ``expected`` is the expected
result.

It is also possible to invoke the ``test`` macro without specifying the
reporting functions: in this case the framework will use the default
reporting functions, i.e. ``print-dot`` for successful tests and ``print-msg``
for failed tests::

 > (define succ-fail (test  
      ("test 1+1=2" (+ 1 1) => 2)
      ("test 2*1=2" (* 2 1) => 2)
      ("test 2+2=3" (+ 2 2) => 3)))
 ..
 'test 2+2=3' failed. Expected 3, got 4

Appendix: a Pythonic ``for`` loop
-------------------------------------------------

In this appendix I will give the solution to the exercise suggested
at the end of `episode #10`_, i.e. implementing a Python-like ``for``
loop.

First of all, let me notice that Scheme already has the functionality
of Python ``for`` loop (at least for lists) via the ``for-each``
construct::

 > (for-each (lambda (x y) (display x) (display y)) '(a x 1) '(b y 2))
 abxy12

The problem is that the syntax looks quite different from the Python
equivalent::

 >>> for (x, y) in (("a", "b"), ("x", "y"), (1, 2)): 
 ...     sys.stdout.write(x); sys.stdout.write(y)

One problem is that the order of the list is completely different, but
this is easy to fix with a ``transpose`` function:

$$TRANSPOSE

[if you have read carefully `episode #8`_ you will notice the
similarity between ``transpose`` and ``zip``].  The ``transpose``
function works as follows::

 > (transpose '((a b) (x y) (1 2)))
 ((a x 1) (b y 2))

Then there is the issue of hiding the ``lambda`` form, but this is an
easy job for a macro::

$$FOR

(the ``1`` suffix means that this is version 1 of our macro, but we
will improve it with successive versions). 

The important thing to notice in this implementation is the usage of a guard 
with an ``else`` clause: that allows to
introduce two different behaviours for the macro at the same time.
If the pattern variable ``el`` is an identifier, then ``for`` is
converted into a simple ``for-each``::

 > (for x in '(1 2 3) (display x))
 123

On the other hand, if the pattern variable ``el`` is a list of
identifiers and ``lst`` is a list of lists, then the macros
also reorganize the arguments of the underlying ``for-each``
expression, so that ``for`` works as Python's ``for``::

 > (for (x y) in '((a b) (x y) (1 2)) (display x) (display y))
 abxy12


Incidentally, ``<literals>`` itself here is implemented as a literal
identifier in ``syntax-match``, so you can use literals to "send
commands" to a macro. I remind you from the precedent episode that
``sweet-macros`` recognize commands like ``<patterns>`` and
``<source>``::

 > (for <patterns>)
 ((for el in lst do something ...))

 > (for <source>)
 (syntax-match (in)
  (sub (for el in lst do something ...)
    #'(for-each (lambda (el) do something ...) lst) (identifier? #'el)
    #'(apply for-each (lambda el do something ...)
        (transpose lst))))

Generally speaking you can define macros responding to any pre-defined set of
commands denoted by literal identifiers, i.e. you can
define object-oriented macros in the message-passing sense.

.. _episode #8:
.. _episode #10:


|#

(import (rnrs) (sweet-macros) (easy-test))

;TRANSPOSE
(define (transpose llist) ; llist is a list of lists
  (apply map (lambda x x) llist)) 
;END

;FOR
(def-syntax for 
  (syntax-match (in)
   (sub (for el in lst do something ...)
    #'(for-each (lambda (el) do something ...) lst) (identifier? #'el)
    #'(apply for-each (lambda el do something ...) (transpose lst)))))
;END
