#|
Micro-introduction to functional programming
=======================================================

The first installment of my long-awaited third cycle of a Pythonista's
adventures with Scheme is devoted to Scheme's functional aspects.

A minimal introduction to functional programming
-----------------------------------------------------------------

I assume you already know what *pure functional language* means: a
language is purely functional if variables cannot be re-assigned,
data structures cannot be modified, and side effects are excluded.

Of course, it is impossible to program with a pure functional
language, since input and output are based on side effects and
you cannot have a sensible program without input and output.
However, a practical functional language can still be as pure as
possible if it is able to confine the non-functional aspects to input and
output only, in a controlled way.

The purest functional language 
out there is probably Haskell; on a lower level of purity we find
the languages of the ML family (SML, OCAML, F#, ...); on a lower
level there is Scheme. Python and Common Lisp are at the same level,
both below Scheme.

I would not consider Python and Common Lisp as
truly functional languages: they are just imperative languages with
some support for functional programming (I mean constructs such as ``map``,
``filter``, ``reduce``, list comprehension, generators, et cetera).
However, there is large gap between an
imperative language with some support for functional programming and a
true functional language.

True functional languages have strong support for recursion
(tail call optimization), for higher order functions and for pattern matching;
moreover, true functional languages are based on immutable data
structures.  Scheme is somewhat less functional than SML and Haskell,
since Scheme lists are mutable, currying_ is not supported by the base
syntax of the language, (it can be implemented via macros, of course)
and generally speaking one uses higher order functions less.

While not pure, Scheme can be quite functional if
you avoid rebinding and you restrict yourself to functional data
structures, and it allows many typical idioms of functional programming
which have no counterpart in imperative programming. We already saw a common
trick in `episode #5`_, i.e. the accumulator trick, which is a
way to avoid mutation in loops by using recursion. In this episodes and
the next ones we will show many others.

.. figure:: diamond.jpg

   The purity of functional languages

.. _Haskell: http://www.haskell.org/
.. _currying: http://en.wikipedia.org/wiki/Currying
.. _episode #5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

Functional data structures: pairs and lists
-----------------------------------------------------------------

Historically, the basic data types of Lisp languages (pairs and
lists) have always been mutable. In all Lisp dialects (including
Scheme) it has always been possible to modify the ``car`` and the
``cdr`` of a pair freely. The situation changed with the
R6RS report: nowadays the imperative procedures ``set-car!`` and
``set-cdr!`` have been removed from the rnrs environment.

Actually, it is still possible to mutate pairs, but only by
extending the rnrs environment, i.e. by importing the 
``(rnrs mutable-pairs)`` extension, which is part of the standard library
but not of the core language. This is a clear indication of the fact
that the functional paradigm is somewhat recommended paradigm in Scheme,
even if it is not enforced (in strongly functional languages, such as SML
and Haskell, lists are immutable and there is no other option).

I should notice that the requirement of importing ``(rnrs
mutable-pairs)`` is only valid for scripts and libraries: the behavior
of the REPL is unspecified in the R6RS document (actually the R6RS
forbids a REPL but every R6RS implementation provides a REPL with some
different semantics) and implementations are free to import or to not
import ``mutable-pairs`` in the REPL. The REPL of Ikarus imports
``mutable-pairs`` by default, so you have at your disposal
``set-car!`` and ``set-cdr!``, but this is an implementation specific
choice; other implementations can behave differently from Ikarus at
the REPL, and in general they do.

Excluding code typed at the REPL, in principle the compiler could use
immutability optimizations for library code not importing the ``(rnrs
mutable-pairs)`` extension.  In practice, a Scheme compiler cannot
perform the optimizations based on the assumption of truly immutable
pairs, because the current standard says that ``cons`` must allocate a
new pair and cannot re-use a previously created one
(i.e. ``(eq? (cons x y) (cons x y))`` is always false for any value of
``x`` and ``y``: even if the two conses have the same value, they
correspond to different objects).

If pairs were really immutable, a compiler could use the same object
for equivalent pairs, i.e.  ``'(x . y)`` could be the same as ``(cons
x y)`` and a compiler could cache that value: with the current
standard that cannot never happen.  The point is well explained in a
recent thread in comp.lang.scheme (`Really immutable pairs`_).

Really immutable pairs (and thus lists) have lots of advantages: I would
welcome them in the standard, but I am not sure if that will happen,
since there is a potential compatibility breaking problem. I can only
hope for the best. Immutability has advantages from the point of view
of efficiency and makes the life of language implementors easier, but
those are not really important point for an application programmer.

The important point for the mere mortals is that programs based on
immutable data structures becomes easier
to understand and to debug. For instance, consider a routine taking
a list in input and suppose that the content of the list is not
what you would expect. If the list is mutable you potentially have to
wonder about you whole code base, since everything could have mutated
the list before reaching the routine you are interested in.
If the list if immutable, you are sure that the bug must be
in the procedure which created the list, and in no
other place.

Moreover, functional structures avoid whole classes of bugs (I am sure
every Pythonista has found some issue with lists being mutable,
especially when used as default arguments) especially in the hairy
situations of multithreaded code.  In my *Adventures* I will never
rely on the ability to mutate pairs, and I will use pairs as
functional data structures.

Functional update
-------------------------------------------------

There is apparently an issue with immutable data structures:
in many imperative programs one needs to
modify the data: but how can you update an immutable object?

The answer is actually pretty simple: you don't. Since you cannot mutate an
immutable object, the only option is to create a brand new object with
a different content from the original one. This mechanism is called
*functional update*: for instance, it is easy to define two functional
procedures ``set-car`` and ``set-cdr`` performing the job of
``set-car!`` and ``set-cdr!`` but without mutation::

 (define (set-car pair value)
   (cons value (cdr pair)))

 > (set-car (cons 1 2) 3)
 (3 . 2)

 (define (set-cdr pair value)
   (cons (car pair) value))

 > (set-cdr (cons 1 2) 3)
 (1 . 3)

What if you want to (functionally) update the *n*-th value of a list?
The trick is to use recursion::

 (define (list-set n lst value)
   (if (zero? n) 
      (set-car lst value) 
      (cons (car lst) (list-set (- n 1) (cdr lst) value))))

 > (list-set 2 '(a b c d) 'X)
 (a b X d)

Notice that ``list-set`` is nicer that its imperative counterpart::

 (define (list-set! n lst value) 
   (set-car! (list-tail lst n) value) lst)

 > (define ls '(a b c d))
 > (list-set! 2 ls 'X)
 > ls
 (a b X d)

Notice the use of the R6RS procedure ``(list-tail lst n)`` which
returns the tail of ``lst`` starting from the n-th element.
The indexing starts from zero, as usual (for
instance ``(list-tail '(a b c d) 2)`` is the list ``(c d)``).
The important bit to understand how ``list-set!`` works
is that ``list-tail`` returns the tail, and *not a copy*
of it: by mutating the tail you are actually mutating the original
list. This is clearly quite risky.

The R6RS standard does not provide primitives for functional update
out of the box (
``list-set!`` is not in the standard since it is of very little
utility: if you want to be able to modify the n-th element of a
sequence, you are much better off by using a vector and not a list).
However, it does provide primitive to remove elements for a list
functionally::

 > (remp even? '(3 1 4 1 5 9 2 6 5)); complementary of filter
   (3 1 1 5 9 5)

 > (remove 1 '(3 1 4 1 5 9 2 6 5)) 
 (3 4 5 9 2 6 5)

 > (remv 1 '(3 1 4 1 5 9 2 6 5)) 
 (3 4 5 9 2 6 5)

 > (remq 'foo '(bar foo baz))       
 (bar baz)

(you can find other `list utilities`_ in R6RS standard library).

One would expect functional update to be much slower that imperative
update, because of the need of creating potentially long lists to
modify just a single element.  However, this is not necessarily
true. It all depends on how smart your compiler is. A smart compiler
can internally use mutation
(at the machine level), so in principle it could be as fast as
imperative code. The advantage is that mutation is managed by the
compiler, not by the programmer, who can think purely in terms of
immutable data structures.

It is true that compiler for functional languages are still slower
than C compilers in average, but they are not so bad. In most
situations the slowdown is acceptable and in particular situations
compilers for functional languages can be faster than a C compiler.
Moreover, they allow for memory optimizations. For instance, you can
memoize an immutable list, whereas you cannot memoize a mutable
one.

Since I come from a Python background I do not care much about
performance and optimizations, but I care a lot about maintainability
of programs and bug reduction, as well as about conceptual cleanness.
Moreover, this series has also some pedagogical intention, therefore
I will prefer functional solutions over imperative ones here, in order
to show new ways of doing old things.

.. _Really immutable pairs: http://groups.google.com/group/comp.lang.scheme/browse_frm/thread/7eccba9fb4eebb44/69241209d0d053bb?hl=en&lnk=gst&q=immutable+pairs#69241209d0d053bb                   
.. _list utilities: http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-4.html#node_chap_3

|#
