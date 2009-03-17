#|
Are macros really useful?
===================================================================

In this episode I discuss the utility of macros for enterprise programmers.

Are macros "just syntactic sugar"?
----------------------------------------------------

.. image:: sugar.jpg

There is a serious problem when teaching macros to beginners: the real
power of macros is only seen when solving difficult problems, but you
cannot use those problems as teaching examples.  As a consequence,
virtually all beginner's introductions to macros are dumbed down:
usually they just show a few trivial examples about how to
modify the Scheme syntax to resemble some other language. I did the same
too. This way of teaching macros has two negative effects:

1. beginners are naturally inclined to pervert
   the language instead of learning it;

2. beginners can easily dismiss macros as mere
   syntactic sugar.

The first effect is the most dangerous: the fact that you can implement a C-like
``for`` loop in Scheme does not mean that you should use it! I
strongly believe that learning a language means learning its idioms:
learning a new language means that you must change the way you think
when writing code. In particular, in Scheme, you must get used to
recursion and accumulators, not to imperative loops, there is no other
way around.

Actually, there are cases
where perverting the language may have business sense. For instance, suppose
you are translating a library from another language with a
``for`` loop to Scheme. If you want to spend a minimal effort in the
translation and if for any reason you want to stay close to the
original implementation (for instance, for simplifying maintenance),
then it makes sense to leverage on the macro
facility and to add the ``for`` loop to the language syntax.

The problem is that it is very easy to abuse the mechanism. Generally
speaking, the adaptibility of the Scheme language is a double-edged
sword.  There is no doubts that it increases the programmer
expressivity, but it can also make programs more difficult to
read. The language allows you to invent your own idioms that
nobody else uses, but perhaps this is not such a good idea if you care
about other people reading your code. For this reason macros in the
Python community have always been viewed with suspicion: I am also
pretty confident that they will never enter in the language.

The second effect (dismissing macros) is less serious: lots of
people understimate macros as mere syntactic sugar, by forgetting that
all Turing complete language differ solely on syntactic sugar.
Moreover, thinking too much about the syntactic sugar aspect make them
blind to others and more important aspects of macros: in particular,
the fact that macros are actually *compilers*.

That means that you can implement
with macros both *compile time checks* (as I have stressed in `episode #10`_,
when talking about guarded patterns) and *compile time computations*
(I have not discussed this point yet) with substantial
benefits for what concerns both the reliability and the performance of
your programs. In `episode #11`_ I have already shown how
you can use macros to avoid expensive function calls in benchmarks
and the example generalizes to any other situations.

In general, since macros allows you to customize the evaluation mechanism
of the language, you can do with macros things which are impossible
without them: such an example is the ``test`` macro discussed
in `episode #11`_. I strongly suggest you to read the third comment
to that episode, whereas it is argued that it is impossible to
implement an equivalent functionality in Python.

So, you should not underestimate the power of macros; on the other
hand, you should also not underestimate the complication of macros.
Recently I have started a `thread on comp.lang.scheme`_ with 180+ messages
about the issues I have encountered when porting my ``sweet-macros``
library between different Scheme implementations, and the thread ended
up discussing a lot of hairy points about macros (expand-time vs run-time,
multiple instantiation of modules, separate compilation, and all that).

.. _thread on comp.lang.scheme: http://groups.google.com/group/comp.lang.scheme/browse_frm/thread/8927053ede92fd27?hl=en#

About the usefulness of macros for application programmers
-----------------------------------------------------------------------------

I am not an advocate of macros for enterprise programming. Actually,
even ignoring the issue with macros, I cannot advocate
Scheme for enterprise programming because of the lack of a standard
library worth of its name. This was more of an issue with R5RS Scheme,
but it is still a problem since Scheme has an extremely small standard
library and no concept of *batteries included* à la Python. As a
consequence, everybody has to invent its own collections of utilities,
each collection a little bit different from the other.

For instance,
when I started learning Scheme I wrote a lot of utilities;
later one, I find out that I could find the same utilites, under
different names and slightly different signatures, in various Scheme
frameworks.  This never happened to me in Python to the same extend,
since the standard library is already coding in an uniform way most of
the useful idioms, so that everybody use the library and there is
less need to reinvent the wheel.

On the other hand, I am not a macro aficionado like Paul Graham, who says:

 *When there are patterns in source code, the response should not be
 to enshrine them in a list of "best practices," or to find an IDE
 that can generate them.  Patterns in your code mean you are doing
 something wrong.  You should write the macro that will generate them
 and call that instead.*

I think Graham is right in the first part of its analysis, but not in
the conclusion. I agree that patterns are a `code smell`_ and I think
that they denote a lack in the language or in its standard library. On
the other hand, the real solution for the enterprise programmer is not
to write her own macro which nobody knows, but to have the feature
included in the language by an authoritative source (for instance
Guido van Rossum in the case of Python) so that *all* users of the
language get the benefit in an uniform way.

This happened recently in Python, with the ternary operator, with the
``try .. except .. finally`` statement, with
the *with statement*, with extended generators and in many other
cases. The Scheme way in which everybody writes his own language
makes sense for the academic researcher, for the solitary hacker,
or for very small team of programmers, but not for the enterprise.

Notice that I am not talking about specialized newly invented
constructs: I am talking about *patterns* and by definition, according
to the GoF_, a pattern cannot be new, it must be a tried and tested solution
to a common problem. If something is so common and well known
to be a pattern, it
also deserves to be in the standard library of the language, or
in a standard framework. This
works well for scripting languages, which have a fast evolution,
and less well in languages designed by committee, where you can
wait years and years for any modernization of the language/library
(we all know Paul Graham is coming from Common Lisp, so his position
is understandable).

In my opinion - and your are free to disagree of course - the
enterprise programmer is much better served by a language without
macros but with a very complete library where all useful constructs
have been codified already.
After all, 99.9% of the times the enterprise
programmer has to do with already solved problems: it is not by
chance that frameworks are so used in the enterprise world. Notice
that by "enterprise programmer" I mean the framework *user*, not
the framework *writer*.

Take my case for instance: at work I am doing some Web programming,
and I just use one of the major Python web frameworks (there already
too many of them!); I do quite of lot of interaction with databases,
and I just use the standard or *de facto* standard drivers/libraries
provided for the task at hand; I also do some scripting task:
then I use the standard library a lot. For all the task I
routinely perform at my day job macros would not help me a bit:
after all Python has already many solutions to avoid boilerplate
(decorators, metaclasses, etc.) and the need for macros is not
felt. I admit that some times I wished for new constructs in Python:
but usually it was just a matter of time and patience to get them in
the language and while waiting I could always solve my problems
anyway, maybe in a less elegant way.

There are good use cases for macros, but there also plenty of workarounds
for the average application programmer.

For instance, a case where one could argue for macros, is when there
are performance issue, since macros are able to lift computations from
the runtime to the compile time, and they can be used for code
optimization. However, even without macros, there is plenty of room
for optimization in the scripting language world, which typically
involve interfacing with C/C++.

There also various standard techniques for *code generation* in C, 
whereas C++ has the infamous *templates*: while those are solutions
very much inferior to Scheme macros, they also have the enormous
advantage of working with an enterprise-familiar technology, and you 
certainly cannot say that for Scheme.

The other good use for macros is to implement compile time checks:
compile time checks are a good thing, but in practice people have
learned to live without by relying on a good unit test coverage, which
is needed anyway.

On the other hand, one should not underestimate the downsides of
macros. Evaluation of code defined inside of the macro body at
compile time or suspension of evaluation therein leads often to bugs
that are hard to track. The behaviour of the code is generally not
easy to understand and debugging macros is no fun.

That should explain why the current situation about Scheme in the
enterprise world is as it is. It is also true that
the enterprise programmer's job is sometimes quite
boring, and you can risk brain atrophy, whereas for sure
you will not incur in this risk if you keep reading my *Adventures* ;)

You may look at this series as a cure against senility!

.. _code smell: http://en.wikipedia.org/wiki/Code_smell
.. _GoF: http://en.wikipedia.org/wiki/Design_Patterns


Appendix: a Pythonic ``for`` loop
-------------------------------------------------

In this appendix I will give the solution to the exercise suggested
at the end of `episode #10`_, i.e. implementing a Python-like ``for``
loop.

First of all, let me notice that Scheme already has the functionality
of Python ``for`` loop (at least for lists) via the for-each_
construct::

 > (for-each (lambda (x y) (display x) (display y)) '(a x 1) '(b y 2))
 abxy12

The problem is that the syntax looks quite different from Python::

 >>> for (x, y) in (("a", "b"), ("x", "y"), (1, 2)): 
 ...     sys.stdout.write(x); sys.stdout.write(y)

One problem is that the order of the list is completely different, but
this is easy to fix with a ``transpose`` function:

$$TRANSPOSE

(if you have read carefully `episode #8`_ you will notice the
similarity between ``transpose`` and ``zip``). ``transpose``
works as follows::

 > (transpose '((a b) (x y) (1 2)))
 ((a x 1) (b y 2))))

Then there is the issue of hiding the ``lambda`` form, but this is an
easy job for a macro:

$$FOR

The important thing to notice in this implementation is the usage of a guard 
with an ``else`` clause: that allows to
introduce two different behaviours for the macro at the same time.
If the pattern variable ``el`` is an identifier, then ``for`` is
converted into a simple ``for-each``::

 > (for x in '(1 2 3) (display x))
 123

On the other hand, if the pattern variable ``el`` is a list of
identifiers and ``lst`` is a list of lists, then the macro
also reorganizes the arguments of the underlying ``for-each``
expression, so that ``for`` works as Python's ``for``::

 > (for (x y) in '((a b) (x y) (1 2)) (display x) (display y))
 abxy12


.. _for-each: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_644
.. _episode #8: http://www.artima.com/weblogs/viewpost.jsp?thread=240793
.. _episode #10: http://www.artima.com/weblogs/viewpost.jsp?thread=240805
.. _episode #11: http://www.artima.com/weblogs/viewpost.jsp?thread=240833
|#

(import (rnrs) (sweet-macros) (easy-test) (only (ikarus) void))

;TRANSPOSE
(define (transpose llist) ; llist is a list of lists
  (if (and (list? llist) (for-all list? llist))
      (apply map list llist)
      (error 'transpose "Not a list of lists" llist)))
;END

;FOR
(def-syntax for 
  (syntax-match (in)
   (sub (for el in lst do something ...)
        #'(for-each (lambda (el) do something ...) lst)
        (identifier? #'el))
   (sub (for (el ...) in lst do something ...)
        #'(apply for-each (lambda (el ...) do something ...) (transpose lst))
        (for-all identifier? #'(el ...))
        (syntax-violation 'for "Non identifier" #'(el ...)
                          (remp identifier? #'(el ...))))
   ))
;END

(run
 (test "transpose" (transpose '((a b) (x y) (1 2)))
       '((a x 1) (b y 2)))
 (test "for1" (for x in '(1 2 3) (display x))
       (void))
 (test "for2" (for (x y) in '((a b) (x y) (1 2)) (display x) (display y))
       (void)))

;(for (x 2) in '((a b) (x y) (1 2)) (display x) (display y))
