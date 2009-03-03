#|

Se qualcuno stilasse una lista dei concetti che hanno avuto più
successo nei linguaggi di programmazioni negli ultimi anni, sicuramente
il concetto di iteratore sarebbe ai primi posti (in questa introduzione
lasciatemi usare un concetto impreciso di iteratore, come di una sequenza
di elementi potenzialmente infinita su cui si può iterare). 
La cosa interessante
è che non si tratta affatto di un concetto recente (era noto nei
circoli della Computer Science da sempre, in linguaggi quali Scheme,
ML, Haskell, Icon eccetera) ma sta diventando di uso
sempre più comune da quando è stato popolarizzato da Python.

Iterators and streams
----------------------------------------------------------

As we saw in the last issue, speaking about list comprehension,
when Python copies an idea from a functional language, it does so
in imperative way. The same happened for Python iterators, which
are somewhat copied from the functional concepts of streams, but
in impure way. Let me explain what I mean with an example; the Python
iterator

 >>> it123 = iter(range(1, 4))

is an object with an internal state; there is a ``next`` method which
allows to change the state. In particular, if you call ``.next()`` twice,
the same iterator returns different values:

 >>> it123.next()
 1
 >>> it123.next()
 2

Thus, Python iterators *are not functional*. The concept that takes
the place of Python iterators in functional languages such as Scheme
ML and Haskell is the concept of *stream*. Stream are superficially
similar to iterator, but they are actually quite difference, since
they have no internal state and there is no mutation involved.  Since
Ikarus comes with a built-in stream library, I can give a concrete
example. Notice that you can use streams in other R6RS implementation
simply by using the reference implementation described in SRFI-41.

Here is how to define a stream on the numbers 1,2,3::

 > (import (streams))
 > (define str123 (stream-range 1 4))

There is no equivalent of the ``next`` method for streams, since
concept of internal state of a stream. However, there is a 
``stream-car`` procedure which takes the first element of a stream,
and a ``stream-cdr`` procedure that returns another stream which
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
.. _streams: http://srfi.schemers.org/srfi-41/srfi-41.html

Stream comprehension and other tricks
-------------------------------------------------------------

The streams_ module offers a series of convenient features. The most
useful one is *stream comprehension*, which is very similar to Python
generator expressions. For instance, in Python we can express the
infinite set of the even number as a genexp

 >>> import itertools
 >>> even = (i for i in itertools.count(0) if i % 2 == 0)

whereas in Scheme we can express it as a stream::

 > (define even (stream-of i (i in (stream-from 0)) (zero? (modulo i 2))))

It is possible to loop over a stream with ``stream-for-each``,
``stream-map``, ``stream-fold`` which work as for lists, but
which return streams, or with ``stream-let`` which is stream version
of named let. Moreover there is a function
``stream->list`` with does the obvious.

For instance, here is a way to sum the first 5 even numbers::

(stream-let loop ((even-numbers even) (i 0) (sum 0))
     (if (>= 5) sum 
         (loop (stream-cdr even) (+ 1 i) (+ (stream-car even-numbers) sum))))

SRFI 41 explains well why using ``let`` instead of ``stream-let`` here
would not work:
Non sto a spiegare tutto, perché trovare documentazione ed esempi
d'uso nel documento dell'SRFI. Una carenza del modulo stream
è l'assenza di una funzione ``stream-equal?``, che comunque
possiamo definirci da soli::

 > (define (stream-equal? eql? xs ys)
     (cond 
      ((and (stream-null? xs)  (stream-null? ys)) #t); both streams are null
      ((or (stream-null? xs)  (stream-null? ys)) #f); only one stream is null
      ((not (eql? (stream-car xs) (stream-car ys))) #f); the first element is different
      (else (stream-equal? eql? (stream-cdr xs) (stream-cdr ys))) ; loop
      ))

Two streams are recognized as different as soon as two different
elements are found; on the other hand, in order to verify the equality
of two streams it is necessary to compare *all* the elements: this is
computationally impossible if the two streams are infinite.
Therefore ``stream-equal?`` should be used with care. For finite streams
which are likely to be equal, it may be more effective to convert them
into lists and to compare directly the latter.

Lazyness is a virtue
----------------------------------------------------------

Let me explain now the basic feature of streams, i.e. lazyness.
Streams are lazy since they perform work only when forced - i.e.
only when an element is explicitly requested -  and even there if
they had already accomplished a task they do not perform it again
- i.e. they memoize the elements already computed.
An example should make these two points clear.
Let us define a ``work`` procedurs which protests when called::

 > (define (work i)
      (display "Life is hard!\n") i)

The protest is expressed an a side effect ``(display "Life is hard! ")``;
other than that, the function, does not perform too much, since it
just returns the parameter got in input, by you know, there is no
limit to lazyness ;)

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
|#
