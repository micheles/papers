#|
In this episode I will give a comparison of sweet-macros with other
Scheme macros, then I will give a general discussion about the utility of
macros for application programmers.

Comparison with other macro systems
------------------------------------------------

I am sure some of my readers are familiar with other Scheme
macros systems, or with Common Lisp ``defmacro``. This section is for
their benefit, to contrast ``sweet-macros`` with other macro systems
they may be familiar with. If a your a total beginner to Scheme macros
you may safely skip this section, that could engender some confusion.

The oldest system of macros available in Scheme is ``define-macro``,
which closely resemble ``defmacro`` in Common Lisp. ``define-macro``
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
it supports guarded patterns out of the box.

Guarded patterns are a relative advanced feature and they
are not available in the ``define-macro`` and in the ``syntax-rules``
macro systems. They are present in the ``syntax-case`` macro system
but in a verbose version, as fenders. Readers familiar with
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

Are Scheme macros "just syntactic sugar"?
----------------------------------------------------

There is a serious problem when teaching macros to beginners: the real
power of macros is seen only when solving difficult problems, but you
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

So, the ability of macros to pervert the language is a potential
issue for beginners. On the other hand, there are cases where perverting
the language may have sense. For instance, suppose you are
translating to Scheme a library in another language with a ``for``
loop. Suppose you want to spend a minimal effort in the translation
and that for any reason you want to stay close to the original
implementation. Then, it makes sense to leverage on the macro facility
to add the ``for`` loop to the language syntax with a minimal effort. 

The problem is that it is very use to abuse macros. Generally speaking,
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

From my previois observations, you may believe me to be a big fan of macros, and
and an advocate of macros for enterprise programming, as a lot of Lispers.
But actually I am not. On one hand, I cannot advocate Scheme for
enterprise programming because of the lack of a standard library
worth of its name. That was more of an issue with
R5RS Scheme, but it is still a problem since Scheme has an extremely
small standard library and no concept of *batteries included* a la
Python. As a consequence, everybody has to invent its own collections
of utilities, each collection a little bit different from the
other. For instance, when I started learning Scheme I wrote a lot of
little utilities; later one, I find out that I could find my same
utilites, under different names and slightly different signatures, in
various Scheme frameworks.  This never happened to me in Python to the
same extend, since the standard library is already coding in an
uniform way most of the useful idioms, so that everybody use the
library and there is much less need to reinvent the wheel.

On the other hand, I am not a macro aficionado like Paul Graham, who says:

 *When there are patterns in source code, the response should not be
 to enshrine them in a list of "best practices," or to find an IDE
 that can generate them.  Patterns in your code mean you are doing
 something wrong.  You should write the macro that will generate them
 and call that instead.*

I think he is right in the first part of its analysis, but not in the
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

So, there many possible alternatives to Scheme in the enterprise
world, and that explain why the current situation is as it is.
On the other, the enterprise programmer's job is sometimes quite
boring, and you can risk brain atrophization, whereas for sure
your incur in this risk if you keep reading my *Adventures* ;)

You may look at this series as a cure against senility!

.. _code smell: http://en.wikipedia.org/wiki/Code_smell
.. _GoF: http://en.wikipedia.org/wiki/Design_Patterns
|#
