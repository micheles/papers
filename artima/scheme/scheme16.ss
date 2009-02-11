#|

let+ versus let-values
---------------------------------------------------------------------------

There is a concept of Scheme that I never liked, i.e. the existence of
functions (more in general continuations) returning multiple values.
This is a debated topic, since old versions of Scheme did not have
the concept (I think multiple values
entered in the standard with the R5RS report) and there was some debate
against it (I do not have an handy reference, but I am sure some of
my readers can provide some background). Unfortunately the idea
entered in the standard, and now it is possible to define functions
returning multiple values at the same time, as in this example::

 > (define (return-three-values)
     (values 1 2 3))
 > (return-three-values)
 1
 2
 3

I see this as a wart of Scheme, a useless complication motivated by
premature optimization concerns. In order to receive the values a
special syntax is need, and you cannot do things like the following::

 > (apply + (return-three-values))
 Unhandled exception 
  Condition components: 
    1. &assertion 
    2. &who: apply 
    3. &message: "incorrect number of values returned to single value context" 
    4. &irritants: ((1 2 3))

Instead, you are forced to use ``let-values``::

 > (let-values (((x y z) (return-three-values))) (list 1 2 3))
 (1 2 3)

In this series I will never use functions returning multiple values,
except the ones in the Scheme standard library (this is why I am
forced to talk about ``let-values``). Instead of using multiple
values, I will return a list of values and I will destructure it
with ``let+``. For instance, I will write

::

 > (let+ (a b) (list 1 2) (printf "~a ~a\n" a b))
 1 2

instead of 

::

 > (let-values (((a b) (values 1 2))) (printf "~a ~a\n" a b))
 1 2

``let+`` is more elegant and more general than ``let-values``:
everything ``let-values`` can do, ``let+`` can do too.
``let+`` is even faster when you want to match "short" lists, with
two or three elements, as it happens usually::

 running stats for (repeat 10000000 (let-values (((x y z) (values 1 2 3))) 'dummy)):
     no collections
     276 ms elapsed cpu time, including 0 ms collecting
     277 ms elapsed real time, including 0 ms collecting
     0 bytes allocated
 running stats for (repeat 10000000 (let+ (x y z) (list 1 2 3) 'dummy)):
     58 collections
     211 ms elapsed cpu time, including 42 ms collecting
     213 ms elapsed real time, including 43 ms collecting
     240016384 bytes allocated

As you see, ``let+`` takes only 211 ms to unpack a list of three elements
ten million times; ``let-values`` would take 276 ms instead.
On the other hand, ``let+`` involves garbage collection
(in our example 24 bytes are allocate per cycle, and thats means 240
million of bytes) and depending on the situations and the implementation
this may cause a serious slowdown. However, those are implementation
details. Conceptually I think the introduction of multiple values
in Scheme was a mistake. I think functions should *always* return
a single value, possibly a composite one (a list, a vector, or anything
else): SML and Haskell follows this principle.

``let+``
also subsumes the functionality of ``let`` and I would use it
a lot in the future.

Further examples of destructuring: opt-lambda
---------------------------------------------------------------------

A weekness of standard Scheme is the lack of function with default
arguments and keyword arguments. In practice, this is a minor weakness
since there many libraries implementing the functionality (although in
different ways, as usual). As an exercise to show the power of
``let+``, here I will implement a syntax to define functions with default
arguments. I will start from an example, to make clear the intended
functionality of the macro, which I will call ``define/opt``.
Let me define a function ``f`` as follows::

  > (define/opt (f x y (opt (u 1) (v 2)) . rest)
      (printf "Required: ~a Optional: ~a Other: ~a\n"
         (list x y) (list u v) rest))

Here ``x`` and ``y`` are required arguments, ``u`` and ``v``
are optional arguments and ``rest`` are variadic arguments.
If you do not provide an optional argument, its default value is
be used instead, and ``f`` behaves as follows::

 > (f 'a 'b 'c 'd 'e 'f)
 Required: (a b) Optional: (c d) Other: (e f)
 > (f 'a 'b 'c)
 Required: (a b) Optional: (c 2) Other: ()
 > (f 'a 'b)
 Required: (a b) Optional: (1 2) Other: ()
 > (f 'a)
 Unhandled exception
  Condition components:
    1. &assertion
    2. &who: apply
    3. &message: "incorrect number of arguments"
    4. &irritants: (#<procedure f> 1)

It is clear that in order to implement the functionality the trick
is to override the defaults of the optional argument with the passed
arguments, if any. To this aim we will need the following helper
function:

$$OVERRIDE-WITH

At this point it is easy to define an ``opt-lambda`` macro doing the
job:

$$OPT-LAMBDA

``define/opt`` is just sugar over ``opt-lambda``:

$$DEFINE/OPT

A ``fold`` macro
----------------------------------------------------------------

Whereas Scheme ``fold-left`` and ``fold-right`` are more readable
than Python ``reduce`` (at least to me) they are not very
readable (still in my opinion). However, Scheme provides macros
to solve readability issues, so let's use them: that
makes a a good exercise in macro programming. Of course, it would
be better to have a more readable syntax in the standard, but
let's stop the complaining.

Here is my take:

$$list-utils:FOLD

Notice the usage of the ``let+`` macro introduced in the previous
episode: that make it easy to work with nested lists and to use
a Python-like syntax. Moreover, notice the usage of the literals
``is`` and ``in`` to enhance readability, and the usage of the
literals ``left`` and ``right`` to avoid writing two separated
macros.

$$TEST-FOLD

With this syntax, the procedure to flatten a nested lists can be
written as follows:

$$list-utils:FLATTEN

The ``fold`` macro is not as powerful as the original
``fold-left`` and ``fold-right`` functions, because it only works
for binary operators (but this cover 99.9% of cases) and also because
it is a macro, i.e. it is not a first order object that can be
passed to procedure.


|#


(import (rnrs) (sweet-macros) (aps list-utils))


;OVERRIDE-WITH
;;(override-with '(a b) '(1 2 3)) => '(a b 3)
(define (override-with winner loser)
  (let ((n (length winner)) (N (length loser)))
    (if (>= n N)
        winner ; winner completely overrides loser
        (append winner (list-tail loser n)))))
;END

;OPT-LAMBDA
(def-syntax opt-lambda
  (syntax-match (opt)
    (sub (opt-lambda (r1 ... (opt (o1 d1) ...) . rest) body1 body2 ...)
    #'(lambda (r1 ... . args)
        (let+ (o1 ... . rest) (override-with args (list d1 ...))
              body1 body2 ...)))))
;END

;DEFINE/OPT
(def-syntax (define/opt (name . args) body1 body2 ...)
  #'(define name (opt-lambda args body1 body2 ...)))
;END


;### OPTIONAL ARGUMENTS http://srfi.schemers.org/srfi-89/srfi-89.html 

