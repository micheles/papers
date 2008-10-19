#|
An introduction to Scheme macros
---------------------------------------------------------

Scheme macros have many faces. For instance, you can see Scheme macros
as a general mechanism to extend the syntax of the base language.
However, you can also see them as a
performance hack to perform arbitrary computations at compile time.
Moreover, you can see them as a clever trick to introduce compile time 
checks in a dynamic language.
I think that the correct way of looking at macros is to see
them as a general facility to write compilers embedded in the Scheme
language. 

Actually, they were intended from the beginning with that goal.
The languages
defined through macros can be very different from Scheme; for instance
you can define object oriented languages (object systems in Scheme or
Lisp are typically built with macros) or even languages with static
typing (the new language Typed Scheme, built on top of PLT Scheme, is
such an example). 

In order to address such use cases, Scheme macros
have to be extremely advanced, much more than Lisp macros and of any
other kind of macros; on the other hand, they also have a reputation
for complexity. Unfortunately, on top of the intrinsic complexity,
Scheme macros also suffers from accidental complexity, due to history
(there are too many standard macro systems in Scheme) and to the
tradition of not caring about readability (many Scheme constructs are
made to look more complex than they actually are).

Scheme has two macro systems
included in the *de jure* standard, plus a *de facto* standard
based on ``define-macro`` system, which is available
in all implementations and it is well known to everybody because of
its strict similarity to Common Lisp ``defmacro`` system.

The two standard macro system are the one based on ``syntax-rules``,
which allows to define hygienic macros only, and the one based
on ``syntax-case``, which allows to define non-hygienic macros too.
``syntax-case`` macros have the full power of Lisp macros and more,
but they are cumbersome to use.

Which macrology should I teach?
---------------------------------------------------

Since there are so many macro systems it is difficult to decide from where
to start in a pedagogical paper or tutorial.

If you look at the original Italian version of this paper, you will
see that there I decided to talk about ``syntax-rules`` macros
first. However, after a lot of thinking, I have decided to go my own
way in this English series of the *Adventures*.  Here I will not
discuss ``syntax-rules``, nor I will discuss ``syntax-case``: instead,
I will discuss my own version of Scheme macros, which I called
sweet-macros_.

.. _sweet-macros: http://www.phyast.pitt.edu/~micheles/scheme/sweet-macros.sls

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
   sugar on top of it and this is what ``sweet-macros`` are for;

3. ``sweet-macros`` are very close to ``syntax-case`` macros, so
   once you understand them you will understand ``syntax-case`` too;
   from there, understanding ``syntax-rules`` is a breeze;

4. starting from ``sweet-macros`` is much better from pedagogical
   purposes, especially for readers with a Common Lisp background,
   since it is easy to explain the relation with ``defmacro`` and
   how to introduce non-hygienic identifiers;

5. my target readers are programmers coming from the scripting
   languages (Perl/Python/Ruby) world. For this kind of public, with
   no previous exposition to Scheme, bare ``syntax-case`` is
   just too hard, so I needed to dress it in nice clothes to make it
   palatable to my audience;

6. ``sweet-macros`` are intended to easier to use than ``syntax-case``
   macros, but they are also more powerful, since they provide
   introspection and debugging capabilities as well as guarded
   patterns out of the box, so they should look attractive to
   experienced users too, but this is a nice side effect and not the
   main motivation for the library;

7. ``sweet-macros`` were written
   expressely for this series of papers, since I did not want to litter
   my explanation of Scheme macros with endless rants. So, I took
   action I wrote
   my own library of macros made "right": this is also a tribute to
   the power of Scheme macros, since you can "fix" them from within
   the standard macro framework.

If you are an advanced reader - a Schemer knowing ``syntax-case/syntax-rules``
or a Lisper knowing ``defmacro`` - I a am sure
you will ask yourself what are the differences
of ``sweet-macros`` with respect to the system you know.
I will make a comparison of the various systems in
the future, in episode #12 and later on. For the moment, you will have
to wait. I do not want to confuse my primary target of readers 
by discussing other macro systems right now.
I also defer to episode #12 the delicate question *are macros a good idea?*.
For the moment, focus on what macros are and how you can use them.
Then you will decide if they are a good idea or not.

Enter sweet-macros
---------------------------------------------

Since the primary goal of ``sweet-macros`` is semplicity, the library
exports only three names, ``def-syntax``, ``syntax-match`` and
``syntax-expand``.

``def-syntax`` is a macro used to define simple macros, which is
similar to ``defmacro``, but simpler and strictly more powerful.
``syntax-match`` is a macro used to define complex macro transformers.
It is implemented as a thin layer of sugar of ``syntax-case``, whereas
``def-syntax`` is implemented as a thin layer of sugar over
``syntax-match``: from that observation you should understand
the reason beneath the name ``sweet-macros`` ;)

Finally, ``syntax-expand`` is a macro which acts as a debugging
facility to expand macros defined via ``def-syntax`` or
``syntax-match``. Such macros also provide some introspection
features. It should be mentioned that standard Scheme macros do not
provide standard debugging and/or introspection facilities and that
every implementation provides different means of debugging
macros. This is unfortunate, since debugging macros is usually
difficult and it is done often, since it is usually difficult to get a
macro right the first time, even if you are an experienced developer.

I wanted to provide the reader of my series with the tools to
understand what they are doing, without relying on the details of the
implementation they are using. Of course, if they want to rely on the
tools of their implementation they can (I hear DrScheme is pretty good
for debugging but I have not tried it since I am an Emacs-addict).

First of all, you should download and install the sweet-macros library::

 $ wget http://www.phyast.pitt.edu/~micheles/scheme/sweet-macros.sls
 $ cp sweet-macros.sls <the-right-place>

The right place depends on your installation; if you are using Ikarus
you should put the library somewhere in you IKARUS_LIBRARY_PATH;
if you are using PLT Scheme you must put it in your collects directory
(on my Mac it is in ``/Users/micheles/Library/PLT Scheme/4.1/collects``).

You can check that the installation went well by importing the
library::

 $ ikarus
 > (import (sweet-macros))

and by trying to define a macro. 

A simple macro: ``multi-define``
----------------------------------------------------

Let me consider the first usage of macros, as a tool to 
extend the base Scheme language with additional constructs.
To give an example, I will introduce a
binding constructs called ``multi-define`` which allows to define
many identifiers at once:

$$MULTI-DEFINE

As you see, Scheme macros are based on `pattern matching`_.
We are giving instructions to the compiler, specifying
how it must acts when it see certain patterns. In or example,
when the compiler sees a ``multi-define`` expression followed by two
sequences with zero o more arguments, it must replace it
with a ``begin`` expression containing a sequence of definitions.
You can check that this is exactly what happens
by means of ``syntax-expand``::

 > (syntax-expand (multi-define (a b) (1 2)))
 (begin (define a 1) (define b 2))

.. _pattern matching: http://en.wikipedia.org/wiki/Pattern_matching

``multi-define`` then works as you would expect::

 > (multi-define (a b) (1 2)) ; introduce the bindings a=1 and b=2
 > a
 1
 > b
 2

You can also introspect the macro: for instance, you can get
the patterns accepted by ``multi-define`` as follows::

 > (multi-define <patterns>)
 ((multi-define (name ...) (value ...)))

Since this is a simple macro it accepts only a single pattern. However,
it is possible to define macros with multiple patterns by relying
on the second form of ``def-syntax``, i.e.

 ``(def-syntax name transformer)``

where the transformer is a procedure which is typically built on
top of ``syntax-match``. For instance, suppose we want to extend
``multi-define`` to work also as a replacement of ``define``, i.e.
suppose we want to accept the pattern ``(multi-define name value)``
where ``name`` is an identifier. Here is how you could do that
by using ``syntax-match``:

$$MULTI-DEFINE2

Here the identifier ``ctx`` denotes the context of the macro, a
concept that I will explain in a future installment; you can
use any valid identitier for the context, including the name
of the macro itself and that is a common convention.
If you are not interested in the context (which is
the usual case, unless you want to define a non-hygienic macro)
you can discard it and use the special identifier ``_`` to make
clear your intent. I leave as an exercise to check that if you
invert the order of the clauses the macro does not work: you
must remember to put the most specific clause *first*.
 
In general you can get the source code for all the macros defined via
``syntax-match``; that includes macros defined via ``def-syntax``,
since internally ``def-syntax`` is implemented in terms of ``syntax-match``.
For instance, the source code (of the transformer) of our original
``multi-define`` macro is the following::

 > (multi-define <source>)
 (syntax-match ()
   (sub (multi-define (name ...) (value ...))
     #'(begin (define name value) ...)))

As you see, for better readability ``def-syntax`` use the name
of the macro for the context, but it is important to realize
that this is optional, any name will do.

We have not explained everything there is to know about
``syntax-match``, but we need to leave something out for
the next episode, right?
|#

(import (rnrs) (sweet-macros))

;MULTI-DEFINE
(def-syntax (multi-define (name ...) (value ...))
  #'(begin (define name value) ...))
;END

(display (multi-define <patterns>))

(multi-define () ())
(multi-define (a) (1))

;MULTI-DEFINE2
(def-syntax multi-define2
  (syntax-match ()
   (sub (ctx (name ...) (value ...))
     #'(begin (define name value) ...))
   (sub (ctx name value)
     #'(define name value))
  ))
;END
