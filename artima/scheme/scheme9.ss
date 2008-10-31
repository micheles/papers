#|
A minimal introduction to Scheme macros
---------------------------------------------------------

Scheme macros have many faces. You can see them
as a general mechanism to extend the syntax of base Scheme, and
also as a mechanism to reduce boilerplate.
On the other hand, if you focus your attention on the fact that they work at
compile time, you can see them as a mechanism to perform arbitrary computations
at compile time, including compile time checks.

I think the correct way of looking at macros is to see them as a
general facility to write compilers for micro-languages - or Domain
Specific Languages, DSL - embedded in the Scheme language.  The
languages defined through macros can be very different from Scheme;
for instance you can define object oriented languages (object systems
such as TinyCLOS_ or Swindle_ are typical examples) or even
languages with static typing (the new language `Typed Scheme`_, built
on top of PLT Scheme, is such an example).

In order to address such use cases, Scheme macros
have to be extremely advanced, much more than Lisp macros and any
other kind of macros I know of; as a consequence, they also have a reputation
for complexity. Unfortunately, on top of the intrinsic complexity,
Scheme macros also suffers from accidental complexity, due to history
(there are too many standard macro systems) and to the
tradition of not caring about readability (many Scheme constructs are
made to look more complex than they actually are).

Scheme has two macro systems
included in the *de jure* standard - ``syntax-rules``,
which allows to define hygienic macros only, and  ``syntax-case``, which
has the full power of Lisp macros and much more - plus a *de facto* standard
based on ``define-macro`` system, which is available
in all implementations and it is well known to everybody because of
its strict similarity to Common Lisp ``defmacro`` system.

.. _TinyCLOS: https://launchpad.net/r6rs-clos
.. _Swindle: http://www.barzilay.org/Swindle/
.. _Typed Scheme: http://www.ccs.neu.edu/home/samth/typed-scheme/

Which macrology should I teach?
---------------------------------------------------

Since there are so many macro systems it is difficult to decide from where
to start in a pedagogical paper or tutorial.
If you look at the original Italian version of this paper, you will
see that I did talk about ``syntax-rules`` macros
first. However, after a lot of thinking, I have decided to go my own
way in this English series of the *Adventures*.  Here I will not
discuss ``syntax-rules``, nor I will discuss ``syntax-case``: instead,
I will discuss my own version of Scheme macros, which I called
``sweet-macros``.

.. _sweet-macros: http://www.phyast.pitt.edu/~micheles/scheme/sweet-macros.zip

Why I am doing that? After all, why my readers should study my own
version of macros when they surely will be better served off by
learning the standard macrology used by everybody? I have spent
*years* debating with myself this very question, but at the end
I have decided to go this way for a series of reasons:

1. I regard the existence of two separate macro systems in the same
   standard as a wart of Scheme and as a mistake made by the R6RS
   editors: they should have included in the language ``syntax-case``
   only, leaving ``syntax-rules`` as a compatibility library
   built on top of ``syntax-case``;

2. I really don't like the ``syntax-case`` syntax, it is by far too
   verbose and unreadable; I find there is a strong need for some
   sugar on top of it and that is what ``sweet-macros`` are for;

3. ``sweet-macros`` are very close to ``syntax-case`` macros, so
   once you understand them you will understand ``syntax-case`` too;
   from there, understanding ``syntax-rules`` is a breeze;

4. starting from ``sweet-macros`` is much better from pedagogical
   purposes, especially for readers with a Common Lisp background,
   since it is easy to explain the relation with ``defmacro`` and
   the hygiene issue;

5. my target readers are programmers coming from the scripting
   languages (Perl/Python/Ruby) world. For this kind of public, with
   no previous exposition to Scheme, bare ``syntax-case`` is
   just too hard, so I needed to dress it in nice clothes to make it
   palatable;

6. ``sweet-macros`` are intended to easier to use than ``syntax-case``
   macros, but they are also more powerful, since they provide
   introspection and debugging capabilities as well as guarded
   patterns, so they should look attractive to
   experienced users too; however, this is a nice side effect and not the
   main motivation for the library;

7. ``sweet-macros`` were written
   expressely for this series of papers, since I did not want to litter
   my explanation of Scheme macros with endless rants. So, I took
   action and I wrote
   my own library of macros made "right": this is also a tribute to
   the power of Scheme macros, since you can "fix" them from within
   the standard macro framework in fifty lines of code.

If you are an advanced reader, i.e. a Schemer knowing
``syntax-case/syntax-rules`` or a Lisper knowing ``defmacro``, I a am
sure you will ask yourself what are the differences of
``sweet-macros`` with respect to the system you know.  I will make a
comparison of the various systems in the future, in episode #12 and
later on. For the moment, you will have to wait.  I do not want to
confuse my primary target of readers by discussing other macro systems
right now.  I also defer to episode #12 the delicate question *are
macros a good idea?*.  For the moment, focus on what macros are and
how you can use them.  Then you will decide if they are a good idea or
not.

Enter sweet-macros
---------------------------------------------

My sweet-macros_ library is a small wrapper
around the ``syntax-case`` macro system. I release it
under a liberal BSD licence. You can do whatever you want with it,
just keep the attribution right.

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/pastine-alle-mandorle.jpg

The primary goal of
sweet-macros_ is semplicity, so it only exports three macros,
``def-syntax``, ``syntax-match`` and ``syntax-expand``:

- ``def-syntax`` is a macro used to define simple macros, which is
  similar to ``defmacro``, but simpler and strictly more powerful.

- ``syntax-match`` is a macro used to define complex macro transformers.
  It is implemented as a thin layer of sugar on top of ``syntax-case``.

- ``syntax-expand`` is a macro which acts as a debugging
  facility to expand macros defined via ``def-syntax`` or
  ``syntax-match``. 

It should be mentioned that standard Scheme macros do not
provide debugging and/or introspection facilities and that
every implementation provides different means of debugging
macros. This is unfortunate, since `debugging macros is usually difficult`_
and it is done often, since it is uncommon to get a
macro right the first time, even if you are an experienced developer.

I wanted to provide my readers with the tools to
understand what they are doing, without relying on the details of the
implementation they are using. Therefore macros defined via ``syntax-match``
(and that includes macros defined via ``def-syntax``) provide out of the box
introspection and debugging features. 

Of course, readers who want to rely on the debugging tools of their
implementation can do so; for instance I hear that DrScheme has a
pretty good macro stepper but I have not tried it since I am an
Emacs-addict.

First of all, you should download and install the right sweet-macros library.
Unfortunately the R6RS module system does not really solve the portability
issue (to my endless frustration) so I had to write different versions
of the same library :-( I you are using Ikarus you should download the
single file version of the library

::

 $ wget http://www.phyast.pitt.edu/~micheles/scheme/sweet-macros.sls

and put it everywhere in you IKARUS_LIBRARY_PATH. If you are using PLT
Scheme (you need a version of PLT newer than 4.0 for R6RS support) you
must download the zip file version

::

 $ wget http://www.phyast.pitt.edu/~micheles/scheme/sweet-macros.zip
 $ unzip sweet-macros.zip
 $ mv sweet-macros <your collects directory>

and install it in your ``collects`` directory, which on my machine is
``$HOME/.plt-scheme/4.0/collects``.

Actually, the multifile version of the library works also with Ikarus
if you have a recent enough version (right now I am using the trunk,
version 0.0.3+, revision 1654). I have not tried the library on Larceny;
I have tried it in Ypsilon Scheme which however has a small bug
so that it does not run there. You should always keep in mind than R6RS
implementations are pretty young and that implementors are still working
to make them really compatible.
I have also prepared an R5RS version which should work in Chicken Scheme,
at least in the interpreter::

 $ wget http://www.phyast.pitt.edu/~micheles/scheme/sweet-macros.scm

However I have developed and tested ``sweet-macros`` in Ikarus only
(*caveat emptor*!). Still, since the title of this blog is *The Explorer*,
I think it is fine if we deal with exploratory code. 

You can check that the installation went well by importing the
library::

 $ ikarus
 > (import (sweet-macros))

and by trying to define a macro.

An example: ``multi-define``
---------------------------------------------------------------

Here is a ``multi-define`` binding construct
which allows to define many identifiers at once:

$$MULTI-DEFINE

As you see, Scheme macros are based on `pattern matching`_:
we are giving instructions to the compiler, specifying
how it must acts when it sees certain patterns. In our example,
when the compiler sees a ``multi-define`` expression followed by two
sequences with zero o more arguments, it must replace it
with a ``begin`` expression containing a sequence of zero or more definitions.
You can check that this is exactly what happens
by means of ``syntax-expand``::

 > (syntax-expand (multi-define (a b) (1 2)))
 (begin (define a 1) (define b 2))

Notice that ``(multi-define () ())`` is valid code expanding to a do-nothing
``(begin)`` expression; if you want to reject this corner case, you should
write your macro as

::

 (def-syntax (multi-define (name1 name2 ...) (value1 value2 ...))
   #'(begin (define name1 value1) (define name2 value2) ...))

so that ``multi-define`` requires one or more arguments. However, it is
often useful to accept degenerate corner cases, because they may simplify
automatic code generation (i.e. ``multi-define`` could appear in the expansion
of another macro).

``multi-define`` works as you would expect::

 > (multi-define (a b) (1 2)) ; introduce the bindings a=1 and b=2
 > a
 1
 > b
 2

I have just scratched the surface of Scheme macros here: I leave the
rest for the next episode, don't miss it!

.. _debugging macros is usually difficult: http://portal.acm.org/citation.cfm?id=1289971.1289994
.. _pattern matching: http://en.wikipedia.org/wiki/Pattern_matching

|#

(import (rnrs) (sweet-macros))

;MULTI-DEFINE
(def-syntax (multi-define (name ...) (value ...))
  #'(begin (define name value) ...))
;END

(display (multi-define <patterns>))

(multi-define () ())
(multi-define (a) (1))

(display (syntax-expand (multi-define (x y) (1 2))))
