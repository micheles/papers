#|
A minimal introduction to functional programming
-----------------------------------------------------------------

I assume you already know what *functional programming* means:
a functional program is a program where variables cannot be re-assigned,
data structures cannot be modified, and side effects are excluded.

Of course, it is impossible to program with a pure functional
language, since input and output are based on side effects and
you cannot have a sensible program without input and output.
However, a practical functional language can still be as pure as
possible if it is able to confine non-functional aspects to input and
output only, in a controlled way.

The purest functional language 
out there is probably Haskell_; on a lower level of purity we find
the languages of the ML family (SML, OCAML, F#, ...); on a lower
level there is Scheme. Python and Common Lisp are at the same level,
both below Scheme. Actually I would not consider Python and Common Lisp as
truly functional languages: they are just imperative languages with
some support for functional programming (i.e. constructs such as ``map``,
``filter``, ``reduce``, list comprehension, generators, et cetera).

I should emphasize that there is large gap between an
imperative language with some support for functional programming and a
true functional language.

True functional languages have strong support for recursion
(tail call optimization); moreover, 
true functional languages are based on immutable data structures and
pattern matching and have strong support for higher order functions.
Scheme is weaker than SML and Haskell, since Scheme lists are mutable,
currying_ is not supported by the base syntax of the language,
(it can be implemented via macros, of course) and generally speaking one
uses higher order functions less.

Moreover, true functional languages have typical idioms which
are missing in imperative programming. We already saw a common
trick in `episode #5`_, i.e. the accumulator trick, which is a
way to avoid mutation in loops by using recursion.

While not pure, Scheme can be quite functional if
you avoid rebinding and you restrict yourself to functional data
structures.

.. _Haskell: http://www.haskell.org/
.. _currying: http://en.wikipedia.org/wiki/Currying
.. _episode #5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

Functional data structures: pairs and lists
-----------------------------------------------------------------

Historically, the basic data types of Lisp languages, i.e. pairs and
lists have always been mutable. In all Lisp dialects (including
Scheme) it has always been possible to modify the ``car`` and the
``cdr`` of a pair freely. The situation started to change with the
R6RS report: nowadays the imperative procedures ``set-car!`` and
``set-cdr!`` have been removed from the rnrs environment.

Actually, it is still possible to mutate pairs, but only by
extending the rnrs environment, i.e. by importing the 
``(rnrs mutable-pairs)`` extension, which is part of the standard library
but not of the core language. This is a clear indication of the fact
that the functional paradigm is a recommended paradigm in Scheme,
even if it is not enforced (in strongly functional languages, such as SML
and Haskell, lists are immutable and there is no other option).

Incidentally, I should notice that the behavior of the REPL is unspecified in
the R6RS document (actually the R6RS forbids a REPL but every R6RS
implementation provides a REPL with some different semantics)
and implementations are free to import or to not
import ``mutable-pairs`` in the REPL. The REPL of Ikarus imports
``mutable-pairs`` by default, so you have at your disposal
``set-car!`` and ``set-cdr!``, but this is an implementation specific
choice; other implementations can behave differently from Ikarus at
the REPL, and in general they do.

The R6RS specifies very well the behavior of scripts and libraries
and it mandates the following:
if you want to mutate a pair in a compiled script or library,
than you *must* import the ``(rnrs mutable-pairs)`` extension.
In principle, that could be used to communicate
to the compiler that it can use immutability optimizations for
code not importing  the ``(rnrs mutable-pairs)`` extension.
In practice, a Scheme
compiler cannot perform the optimizations based on the assumption
of truly immutable pairs, because the current standards says that ``cons``
must allocate a new pair and cannot re-use a previously created one
(i.e. ``(eq? (cons x y) (cons x y))`` is always false for any value
of ``x`` and ``y``: even if the two conses have the same value, they
correspond to different objects). If pairs were really immutable,
a compiler could use the same object for equivalent pairs, i.e.
``'(x . y)`` would be the same as
``(cons x y)`` and a compiler could cache that value:
with the current standard that cannot never happen.

The point is well explained in
a recent thread in comp.lang.scheme (`Really immutable pairs`_).

Truly immutable pairs (and thus lists) have lots of advantages: I would
welcome them in the standard, but I am not sure if that will happen
(there is a potential compatibility breaking problem). I can only
hope for the best. Immutability has advantages from the point of view
of efficiency and makes the life of language implementers easier, but
those are not really important point for an application programmer.

The important point for the mere mortals is that programs based on
immutable data structures becomes easier
to understand and to debug. Suppose you are debugging a routine taking
a list in input and suppose that the content of the list is not
what you would expect. If the list is mutable you potentially have to
wonder about you whole code base, since everything could have mutated
the list before reaching the routine you are interested in.
If the list if immutable, you are sure that the bug must be
in the procedure which created the list, and in no
other place. Also immutability means less bugs, less problems with
multithreaded code, etc.

In my *adventures* I will never rely on the ability to mutate pairs,
and I will consider pairs to be truly immutable i.e. functional data structures.


Functional update
-------------------------------------------------

There is apparently an issue with immutable data structures:
in many imperative algorithms one needs to
modify a list: but how can you update an immutable object?

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

 (define (set-element n lst value)
   (if (zero? n) 
      (set-car lst value) 
      (cons (car lst) (set-element (- n 1) (cdr lst) value))))

 > (set-element 2 '(a b c d) 'X)
 (a b X d)

Notice that set-element is much nicer that its imperative counterpart::

 (define (set-element! n lst value) 
   (set-car! (list-tail lst n) value) lst)

 (define ls '(a b c d))
 > (set-element! 2 ls 'X)
 (a b X d)

R6RS does not provide primitives for functional update out of the box,
however it does provide primitive to remove elements for a list
functionally::

 > (remp even? '(3 1 4 1 5 9 2 6 5)); complementary of filter
   (3 1 1 5 9 5)

 > (remove 1 '(3 1 4 1 5 9 2 6 5)) 
 (3 4 5 9 2 6 5)

 > (remv 1 '(3 1 4 1 5 9 2 6 5)) 
 (3 4 5 9 2 6 5)

 > (remq 'foo '(bar foo baz))       
 (bar baz)

One would expect functional update to be much slower that imperative
update, because of the need of creating potentially long lists to
modify just a single element.
However, this is not necessarily true. It all depends on how
smart your compiler is. A smart compiler can internally use mutation
(at the machine level), so in
principle it could be as fast as imperative code. The advantage is
that mutation is managed by the compiler, not by the programmer, who
can think purely in terms of immutable data structures.

Functional structures have many advantages in terms of simplicity of
usage and they avoid whole classes of bugs (I am sure every Pythonista
has found some issue with lists being mutable, especially when used as
default arguments); moreover they allow for
optimizations. For instance, you can memoize an immutable list, whereas
you cannot memoize a mutable one. Internally, a compiler is allowed to
memoize immutable data structures at will, so it using immutable data
structures does not necessarily means a large memory consumption.

It is true that compiler for functional languages are still slower
than C compilers in average, but 

1. there are exceptions and in particular situations compilers for
   functional languages can be faster than a C compiler;
2. modern compilers for functional languages are not so bad and in most
   situation their performance is within a factor of two from C compilers.

Since I come from a Python background I do not care about performance anyway,
whereas I care a lot about maintainability of programs and bug reduction, so
I will use the functional paradigm whenever possible in this series.

.. _Really immutable pairs: http://groups.google.com/group/comp.lang.scheme/browse_frm/thread/7eccba9fb4eebb44/69241209d0d053bb?hl=en&lnk=gst&q=immutable+pairs#69241209d0d053bb                   
.. _list utilities: http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-4.html#node_chap_3

|#
