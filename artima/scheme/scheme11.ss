#|
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
for the value of ``start``, which is computed twice) in a variable::

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

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/feu_rouge.jpg

@@easy-test.sls

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

Appendix: comparison with other macro systems
------------------------------------------------

I am sure some of my readers are familiar with other Scheme
macros systems, or with Common Lisp ``defmacro``. This section is for
their benefit, to contrast ``sweet-macros`` with other macro systems
they may be familiar with. If a your a total beginner to Scheme macros
you may safely skip this section, that could engender some confusion.

The oldest system of macros available in Scheme is ``define-macro``,
which closely resemble ``defmacro`` in Common Lisp. Usually ``define-macro``
is available on all implementation, and if it is not, you can 
always implement it yourself in terms of ``def-syntax`` in two-lines.
However that requires a deeper knowledge of macros which I will 
demand to the third cycle of my *Adventures*.
``define-macro``
does not offer pattern matching features, so if you need them
as in our ``multi-define`` example you must implement them
yourself. For instance, you could do the following::

 (define-macro (multi-define names values)
   `(begin ,@(map (lambda (n v) `(define ,n ,v)) names values)))

Note: if your system does not implement ``define-macro`` already, you can
implement it yourself in terms of ``def-syntax`` in two-lines, but I
will show how to do that in a future installment.

The simplest macro system in Scheme is ``syntax-rules``, which is based on
pattern matching. With that system ``multi-define`` can be defined as
follows::

 (define-syntax multi-define
   (syntax-rules ()
    ((_ (name ...) (value ...))
    (begin (define name value) ...)))) 

This is basically the same definition as in the ``sweet-macro``
system, a bit more verbose and without any introspection/debugging
capability, but without funny ``#'`` characters (we will explain
the meaning of the ``#'`` reader syntax in a future episode).

The most advanced system available in Scheme is ``syntax-case`` 
which is also the most verbose macro system::

 (define-syntax multi-define
  (lambda (x)
   (syntax-case x ()
    ((ctx (name ...) (value ...))
    #'(begin (define name value) ...)))))

Here you see the funny ``#'`` syntax, as in the ``def-syntax``
example, since ``sweet-macros`` are built directly on top
of ``syntax-case``; you see however that ``def-syntax`` is
much easier to read and also strictly more powerful, since
it implements a stronger version of guarded patterns than 
``syntax-case``.

Guarded patterns are a relative advanced feature and they
are not available in the ``define-macro`` and in the ``syntax-rules``
macro systems. They are present in the ``syntax-case`` macro system
but in a slightly less powerful version, as fenders. Readers familiar with
``syntax-case`` will be interested in knowing that internally
``sweet-macros`` work by generating the appropriate fenders
for ``syntax-case``; for instance our example is compiled
down to something like

::

 (define-syntax multi-define
   (lambda (x)
     (syntax-case x ()
      ((multi-define (name ...) (value ...))
       (= (length #'(name ...)) (length #'(value ...)))
       #'(begin (define name value) ...))
      ((multi-define (name ...) (value ...))
       (syntax-violation 'multi-define 
        "The number of names and values does not mismatch" 
        #'((name ...) (value ...))))
     ))) ;; plus other stuff for introspection
       
which is definitely less readable than the ``def-syntax`` version.

It is worth noticing that the position of the guards in
``syntax-match`` (or ``def-syntax``) is *different* from the position of
the fenders in ``syntax-case``. 
This is not a gratuitous difference. I have spent a lot
of time pondering if I should keep the original ordering or not and I
have decided to change it because:

1. I feel having the guard in the third position is more consistent;
   in this way the first position in a ``syntax-match`` clause is
   always kept by the pattern and the second  position is always
   occupied by the skeleton, whereas in ``syntax-case`` sometimes
   the second position is occupied by a skeleton and sometimes by
   a fender and this is really ugly;

2. I have seen other functional languages with guarded patterns, and
   they kept the guard in the third position;

3. keeping the guard in the third position makes easier to write
   higher order macros, i.e. macros expanding to ``syntax-match``
   transformers;

4. I see no reason to keep a strict compatibility with
   ``syntax-case``: after all, ``sweet-macros`` are intended for users
   knowing nothing about ``syntax-case``.

``sweet-macros`` are pretty sweet, however, and they may appeal to
``syntax-case`` experts too; to avoid make them too similar to
``syntax-case`` I have decided to use the ``sub`` syntax for each
clause: in this case it is impossible to confuse a ``syntax-match``
clause for a ``syntax-case`` clause, and it is obvious to the reader
that she should not expect a one-to-one correspondence to
``syntax-case``, nor the same ordering of the fenders.  Moreover, I
find that the ``sub`` makes the clauses stand out more clearly
and enhance readability.
|#
