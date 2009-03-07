#|
More on list comprehension
-----------------------------------------------------------

In the latest issue I have introduced a syntax for list comprehension
and I have shown a few examples of its usage. However, I have not discussed one
of its most convenient features, i.e. the ability to define
internal variables with the ``(name is value)`` syntax.
I should give an example of that, and I have decided to
take the occasion to show you a solution of the infamous
`eight-queens puzzle`_ that you
will find in all textbooks about programming.

In my opinion the eight queens
puzzle is not so interesting, it is just a typical academic example, good
for fans of riddles.
On the other hand, if you want to study Scheme, you will find this
kind of examples everywhere, so I made my concession to the
tradition. In particular, the main subject of this episode are Scheme streams,
i.e. SRFI-41, and yoy will find a solutions of the eight queens puzzle
in the documentation of SRFI-41, using the same algorithm but streams
instead of lists.

.. figure:: http://upload.wikimedia.org/wikipedia/commons/1/1f/Eight-queens-animation.gif

  Animation taken from Wikipedia

The algorithm is based on a trick which is quite common in Mathematics:
the clever idea
consists in introducing an additional degrees of freedom
which apparently makes the problem harder, but actually gives us a fast
lane to find a solution. Here the trick is to change the question and
to considered not a square chessboard, but a family of rectangular
chessboards with *n* rows and *N* columns (with *n<=N* and *N=8*).
We are interested in the *n=8* solution; however, keeping *n* generic
helps us, since an easy solution can be found for small *n* (in particular
for *n=1*) and we can figure out a recursive algorithm to build the *n+1*
solution starting from the *n*, until we reach *n=8*.

A solution is expressed
as a list of column positions for the queens, indexed by row. In
particular the case *n=1*
(putting a queen on a *1x8* chessboard) has 8 solutions, expressible as
the list of lists ``'((0)(1)(2)(3)(4)(5)(6)(7))`` - let me enumerate
the columns starting from zero. That means that the first (and only)
queen can be at column 0, 1, 2, 3, 4, 5, 6 or 7. If there are two
queens (*n=2*) one has more solutions; for instance the first
queen (i.e. the one at row 0) could be at column 0 and the second
queen (i.e. the one at row 1) at column 2, and a solution is ``(0
2)``. The solutions for the *n*-queens problem are found by looking at
the possible new configurations, starting from the solutions of the
*n-1*-queens problem and by discarding the forbidden ones.

A configuration
is forbidden if two queens are on the same column (by construction
they cannot be in the same row, since they are indexed by row) or
on the same diagonal. The condition *being on the same diagonal*
translates into *the difference between the row coordinates
is the same as the difference between the column coordinates, in absolute
value*. Here is condition expressed in Scheme and making use of ``list-of``
and of the ``is`` syntax, where ``new-row`` and ``new-col`` is the tentative
position of the *n*-th queen and ``safe-config`` is a solution of the
*n-1*-queen problem:

$$SAFE-QUEEN?

``safe-queen?`` checks that the new configuration is safe by looking
at all the queens already placed, with column positions given by the
list safe-config. We can find all the solutions with a
recursive function:

$$QUEENS

In particular you can check that the *n=8* problem has 92 solutions::

 > (length (queens 8 8))
 92

I refer you to Wikipedia for nice drawings of the solutions.

.. _eight-queens puzzle: http://en.wikipedia.org/wiki/Eight_queens_puzzle
.. _found on Wikipedia: http://en.wikipedia.org/wiki/Eight_queens_puzzle_solutions

Iterators and streams
----------------------------------------------------------

To a Pythonista, Scheme streams may look like a version of Python
iterators. However that would be a wrong impression. When Python
copies from functional languages, it does so in an imperative way. Let
me explain with an example. The Python iterator

 >>> it123 = iter(range(1, 4))

is an object with an internal state; there is a ``next`` method which
allows to change the state. In particular, if you call ``.next()`` twice,
the same iterator returns different values:

 >>> it123.next()
 1
 >>> it123.next()
 2

Thus, Python iterators *are not functional*.  Functional languages
such as Scheme ML and Haskell have no imperative iterators: they have
*streams* instead.  In particular, Ikarus comes with a built-in stream
library, so that I can give a concrete example right now (of course
you can use streams in other implementations simply by using the
reference implementation described in SRFI-41_).  Here is how to define
a stream on the numbers 1,2,3::

 > (import (streams))
 > (define str123 (stream-range 1 4))

There is no equivalent of the ``next`` method for streams, since there is
no concept of internal state of a stream. However, there is a 
``stream-car`` procedure which takes the first element of a stream,
and a ``stream-cdr`` procedure returning another stream which
lacks the first element. Both procedures are functional, i.e. they
act without mutating the original
stream object in any way. In particular, if you apply ``stream-car`` twice, you
get always the same result::

 > (stream-car str123)
 1
 > (stream-car str123)
 1

In Python, looping on an iterator exhausts it, and running twice the same
loop can have unexpected results:

 >>> chars = iter('abc')
 >>> for c in chars: print c,
 ...
 a b c
 >>> for c in chars: print c,
 ...

The first time the loop prints "a b c", but the second time it
does not print anything. In a functional language the same code
must have the same effect, it cannot depend from the inner state
of the stream. Actually, this is what happens::

 > (define chars (stream "a" "b" "c"))
 > (stream-for-each display chars)
 abc> (stream-for-each display chars)
 abc> 

.. _SRFI-41: http://srfi.schemers.org/srfi-41/srfi-41.html

Stream comprehension and more
-------------------------------------------------------------

The SRFI-41_ offers a series of convenient features. The most
useful one is stream comprehension, which is very similar to the
list comprehension I discussed in the previous episode. This is
not by accident, since I copied the list comprehensions syntax
from the work of Phil Bewig, which is also the author of the stream
library. The difference between list comprehensions and stream comprehension
is that stream comprehension is lazy and can be infinite. This is very
very similar to Python generator expressions (*genexps*).
For instance, in Python we can express the
infinite set of the even number as a genexp

 >>> import itertools
 >>> even = (i for i in itertools.count(0) if i % 2 == 0)

whereas in Scheme we can express it as a stream::

 > (define even (stream-of i (i in (stream-from 0)) (zero? (modulo i 2))))

It is possible to loop over a stream with ``stream-for-each``,
``stream-map`` and ``stream-fold``; such higher order functions work as
they counterpats for lists, but they. return streams. There is also a
``stream-let`` syntax, which is stream version
of named let, useful when applying the accumulator pattern to streams,
and a function ``stream->list`` with does the obvious.

I am not explaining all the fine details, since the documentations of
the SRFI is pretty good and exhaustive. As I anticipated, there is
also a solution of the eight queen problem using streams that you may
look at.  The difference between the stream solution and the list
comprehension solution is that the first one is lazy, i.e. one get one
solution at the time, on demand, whereas the second one is eager: it
computes all the solutions in a single bunch, and returns them
together.

A shortcoming of the ``stream`` module
is the lack of a ``stream-equal?`` function, which we may define as follows:

$$STREAM-EQUAL?

Two streams are recognized as different as soon as two different
elements are found; on the other hand, in order to verify the equality
of two streams it is necessary to compare *all* the elements: this is
computationally impossible if the two streams are infinite.
Therefore ``stream-equal?`` should be used with care. For finite streams
which are likely to be equal, it may be more effective to convert them
into lists and to compare directly the latter.

Lazyness is a virtue
----------------------------------------------------------

The basic feature of streams, apart from immutability, is lazyness.
Streams are lazy since they perform work only when forced - i.e.
only when an element is explicitly requested -  and even there if
they had already accomplished a task they do not perform it again
- i.e. they memoize the elements already computed.
An example should make these two points clear.
Let us define a ``work`` procedurs which protests when called::

 > (define (work i)
     (display "Life is hard!\n") i)

The protest is expressed an a side effect;
other than that, the function, does not perform too much, since it
just returns the parameter got in input, but, you know, there is no
limit to lazyness!

Now let me define a stream which invokes the function ``work`` three times::

 > (define work-work-work (stream-of (work i) (i in (stream-range 1 4))))

Since the stream is lazy, it does not perform any work at definition
time. It starts working only when elements are expressly required::

 > (stream->list work-work-work)
 Life is hard!
 Life is hard!
 Life is hard!
 (1 2 3)

Now the values 1, 2, 3 have been memoized: if we try to loop again on the
stream, it will return the precomputed values::

 > (stream->list work-work-work)
 (1 2 3)

This shows clearly that the function ``work`` is not called twice.
It is also clear that, had ``work`` some side effect (such as writing
a log message) then using a stream would not be a good idea,
since you could lose some message. Streams are a functional data
structure and it is good practice to use only pure functions, i.e.
functions without side effects. Actually, this is a best practice
always, not only when working with streams!

I could say more about streams and for many problems one can use
streams instead of generators. On the other hand, in Scheme is also
possible to define Python-like generators, and one could define
a generator expression syntax without streams, but using continuations
instead. However, investigating that direction will astray us from our
path. The intention of this third cycle of *Adventures* was just to give
a feeling of what does it mean to be a true functional language, versus
being an imperative language with a few functional-looking constructs.

There are many resources that you may use to deepen your knowledge of Scheme;
and you can always wait for new *Adventures*. But now a cycle ends and a
new cycle begins: the subject will be macros, again.

.. _delay and force: http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-20.html#node_idx_1296
|#
(import (rnrs) (streams) (aps compat) (aps list-utils))

;;STREAM-EQUAL?
(define (stream-equal? eql? xs ys)
  (cond 
   ((and (stream-null? xs)  (stream-null? ys)) #t); both streams are null
   ((or (stream-null? xs)  (stream-null? ys)) #f); only one stream is null
   ((not (eql? (stream-car xs) (stream-car ys))) #f); the first element is different
   (else (stream-equal? eql? (stream-cdr xs) (stream-cdr ys))) ; loop
   ))
;;END

;;SAFE-QUEEN?
(define (all lst) ;; a Python-like all, true if all is true
  (for-all (lambda (x) x) lst))

(define (safe-queen? new-row new-col safe-config)
  (all (list-of (not (or same-col? same-diag?))
                ((row col) in (enumerate safe-config))
                (same-col? is (= col new-col))
                (same-diag? is (= (abs (- col new-col)) (abs (- row new-row))))
                )))
;;END

;;QUEENS
(define (queens n N)
  (if (zero? n) '(())
      (list-of (append safe-config (list col))
               (n-1 is (- n 1))
               (safe-config in (queens n-1 N))
               (col in (range N))
               (safe-queen? n-1 col safe-config)
               )))
;;END
(assert (= 92 (length (queens 8 8))))
