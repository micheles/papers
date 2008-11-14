#|
The point about macros
-----------------------------------------------------------

In the previous three episodes I have given a few examples of
macros:

- macros expanding to definitions (i.e. ``multi-define``);
- macros expanding to plain expressions (i.e. ``for``);
- macros expanding to functions (i.e. ``test``).

It is now the time to make the point and to raise the question
"are macros really useful for the application programmer?".



Are Scheme macros "just syntactic sugar"?
----------------------------------------------------

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/sugar.jpg

There is a serious problem when teaching macros to beginners: the real
power of macros is only seen when solving difficult problems, but you
cannot use those problems as teaching examples.
As a consequence, virtually all existing introductions to macros (including
my own) are forced to use very trivial examples about how to modify
the Scheme syntax to resemble some other language more familiar to the
beginner. That has two negative effects:

1. beginners are naturally inclined to pervert
   the language instead of learning it;

2. beginners can easily dismiss macros as mere
   syntactic sugar.

The first effect is the most dangerous: the fact you can implement a C-like
``for`` loop in Scheme does not mean that you should use it! I
strongly believe that learning a language means learning its idioms:
learning a new language means that you must change the way you think
when writing code. In particular, in Scheme, you must get used to
recursion and accumulators, not to imperative loops, there is no way
out.

The problem is that it is very easy to abuse macros. Generally speaking,
the adaptibility of the Scheme language via the general mechanism of
macros is a double-edged sword.  There is no doubts that it increases
the programmer expressivity, but it can also make programs more
difficult to read. Yes, the language allows you to invent your own
idioms that nobody else uses, but perhaps this is not such a good idea
if you care about other people reading your code. For that reason
macros in the Python community have always been viewed with suspicion:
I am also pretty confident that they will never enter in the
language. The problem is that often macros are used to re-invent the
same concepts with different syntaxes and the maintenance of programs
written by other people is made more difficult without any clear
benefit.  This is a very delicate point which is source of endless
debates on the newsgroups, so I will have to handle it with care.

On one hand, there are cases
where perverting the language may have sense. For instance, suppose
you are translating to Scheme a library in another language with a
``for`` loop. Suppose you want to spend a minimal effort in the
translation and that for any reason you want to stay close to the
original implementation - for instance, for simplifying maintenance.
Then, it makes sense to leverage on the macro
facility to add the ``for`` loop to the language syntax with a minimal
effort.

At this level, even the second effect enters in the game: lots of
people understimate macros as mere syntactic sugar, by forgetting that
all Turing complete language differs solely on syntactic sugar.
Moreover, thinking too much about the syntactic sugar aspect make them
blind to others and more important aspects of macros: in particular,
the fact that macros are actually compilers, so that you can implement
with them both *compile time checks* (as I have stressed in episode
#11) and *compile time computations* (in episode #11 I have shown how
you can use macros to avoid expensive function calls in the
benchmarking example; in a future episode I will show how you can
perform generic computation at compile time) ) with substantial
benefits for what concerns both the reliability and the performance of
your programs.

About the usefulness of macros for application programmers
-----------------------------------------------------------------------------

From my previous observations, you may believe that I am a big fan of
macros, an advocate of macros for enterprise programming, as a
lot of Lispers are.  But actually I am not.

On one hand, I cannot advocate
Scheme for enterprise programming because of the lack of a standard
library worth of its name. That was more of an issue with R5RS Scheme,
but it is still a problem since Scheme has an extremely small standard
library and no concept of *batteries included* a la Python. As a
consequence, everybody has to invent its own collections of utilities,
each collection a little bit different from the other.

For instance,
when I started learning Scheme I wrote a lot of little utilities;
later one, I find out that I could find my same utilites, under
different names and slightly different signatures, in various Scheme
frameworks.  This never happened to me in Python to the same extend,
since the standard library is already coding in an uniform way most of
the useful idioms, so that everybody use the library and there is much
less need to reinvent the wheel.

On the other hand, I am not a macro aficionado like Paul Graham, who says:

 *When there are patterns in source code, the response should not be
 to enshrine them in a list of "best practices," or to find an IDE
 that can generate them.  Patterns in your code mean you are doing
 something wrong.  You should write the macro that will generate them
 and call that instead.*

I think Graham is right in the first part of its analysis, but not in the
conclusion. I agree that patterns are a `code smell`_ and I think they denote
a lack in the language or in its standard library. On the other hand,
the real solution for an enterprise programmer is not to write
her own macro which nobody knows, but to have the feature included in the
language by an authoritative source (for instance Guido van Rossum
in the case of Python) so that *all* users of the language get
the benefit in an uniform way.

This happened recently in Python, with the ternary operator, with the
``try .. except .. finally`` statement, with
the *with statement*, with extended generators and in many other
cases. The Scheme way in which everybody writes his own language
makes sense for the academic researcher, for the solitary hack,
or for very small team of programmers, but not for the enterprise.

Notice that I am talking about specialized newly invented
constructs: I am talking about patterns and by definition, according
to the GoF_, a pattern cannot be new, it must be tried a tested solution
to a common problem. If something is so common and well known
to be a pattern, it
also deserves to be in the standard library of the language, or
in a de facto standard framework. This
works well for scripting languages, which have a fast evolution,
and less well in languages designed by committed, where you can
wait years and years for any modernization of the language/library.

In my opinion - and your are free to disagree of course - the
enterprise programmer is much better served by a language without
macros but with a very complete library where all useful constructs
have been alredy codified.
After all in 99.9% of the time the enterprise
programmer has to do with already solved problems, it is not by
chance that frameworks are so used in the enterprise world.

Take my case for instance: at work I am doing some Web programming,
and I just use one of major Python web frameworks (there already
too many of them!); I do quite of lot of interaction with database,
and I just use the standard or semi-standard drivers/libraries
provided for the task at hand; I also do some scripting task:
then I use the standard library a lot. For all the task I
routinely perform at my day job macros would not help me a bit:
after all Python has already many solutions to avoid boilerplate
(decorators, metaclasses, etc.) and the need for macros is not
felt. I admit that some times I wished for new constructs in Python:
but usually it was just a matter of time and patience to get them in
the language and while waiting I could always solve my problems
anyway, maybe in a less elegant way.

There are good use cases for macros, but those use cases are not
compelling for the average application programmer.

For instance, a case where one could argue for macros, is when there
are performance issue, since macros are able to lift computations from
the runtime to the compile time, and they can be used for code
optimization. However, even without macros, there is plenty of room
for optimization in the scripting language world, which typically
involve interfacing with C/C++.

There also various standard techniques for *code generation* in C, 
whereas C++ has the infamous *templates*: while those are solutions
very much inferior to Scheme macros, they also have the enormous
advantage of working with commonly used languages, and you 
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
 ((a x 1) (b y 2))))

Then there is the issue of hiding the ``lambda`` form, but this is an
easy job for a macro:

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
identifiers and ``lst`` is a list of lists, then the macro
also reorganizes the arguments of the underlying ``for-each``
expression, so that ``for`` works as Python's ``for``::

 > (for (x y) in '((a b) (x y) (1 2)) (display x) (display y))
 abxy12


.. _episode #8: http://www.artima.com/weblogs/viewpost.jsp?thread=240793
.. _episode #10: http://www.artima.com/weblogs/viewpost.jsp?thread=240805
|#

(import (rnrs) (sweet-macros) (easy-test))

;TRANSPOSE
(define (transpose llist) ; llist is a list of lists
  (if (and (pair? llist) (not (list? (car llist))))
      (error 'transpose "Not a list of lists" llist))
  (apply map list llist)) 
;END

;TEST-TRANSPOSE
(test ("transpose" (transpose '((a b) (x y) (1 2))) => '((a x 1) (b y 2))))
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
        (syntax-violation 'for "Non identifier" #'(el ...)))
   ))
;END

(test (success print-nothing) (failure print-msg)
      ("1+1=2" (+ 1 1) 2)
      ("2*1=2" (* 2 1) 2)
      ("2+2=3" (+ 2 2) 3))

