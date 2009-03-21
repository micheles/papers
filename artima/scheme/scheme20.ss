#|
Can a language be both easy and powerful?
-----------------------------------------------------------------------

When it comes to designing programming languages, easy of use and
power seems to go in opposite directions. There are plenty of examples
where something went wrong, i.e. simple languages which however are
good only for teaching and not for professional use, and
professional languages which are however too tricky to use
for the casual programmer. We have also examples of languages which
are both weak in power *and* difficult to use (insert your chosen
language here).

Nevertheless, I think it is perfectly possible to design a language
which is both easy to use and powerful. For instance, Python is a good
example of such language (others will prefer Ruby, or Scala, or
anything else they like).

There are various reasons why Python can be both easy to use and powerful,
the most important ones being the following, in my opinion:

1. it is a one-man language (i.e. it is not a comprimise language made by a
   committee);

2. it is language made from scratch, with no preoccupations of backward
   compatibility;

3. between (premature) optimization and easy of use Python always chooses
   the latter;

4. it provides special syntax/libraries for common operations.

Scheme does not share any of these characters, and as a consequence it
is definitively not an easy language. It is just a powerful language.

However, it is powerful enough that you can make it easy to use, but
that requires (a lot of work) on the part of the programmer, which must
implement point 4 by himself, whereas
nowadays we are all spoiled and we expect the language implementors to
do this kind of work for us.

.. image:: bikeshed.jpg
 :class: right
 :width: 400

I think the explanation for the current situation in Scheme is more historical
and social than technical. On one side, a lot of people in the Scheme
world want Scheme to stay the way it is, i.e. a language for language
experimentations and research more than a language for enterprise
work.  On the other side, the fact that there are so many
implementations of Scheme makes difficult/impossible to specify too
much: this the reason why there are no standard debugging tools for
Scheme, but only implementation-specific ones.

Finally, there is the infamous `bikeshed effect`_ to take in account.
The bikeshed effect is typical of any project designed by a committee:
when it comes to proposing advanced functionalities that very few
can understand, it is easy to get approval from the larger community.
However, when it comes to simple functionality of common usage, everybody
has got a different opinion and it is practically impossible to get
anything approved at all.

To avoid that, the standard does not provide
directly usable instruments: instead, it provides general instruments
which are intended as building blocks on that of which everybody can
write the usable abstractions he/she prefers. Most people nowadays
prefer to have ready-made solutions, because they have deadlines,
projects to complete and no time nor interest in writing things
that should be made by language designers, so that Scheme is little
used in the enterprise world.

There are other options, however, if you are interested in a Scheme
for usage in the enterprise world. You can just use a Scheme
implementation running on the .NET or the Java platform, or a Scheme-like
language such as Clojure_. Clojure runs on the Java Virtual Machine,
it is half lisp and half Scheme, it has a strong functional flavour in
it, it has interesting things to say about concurrency_,
it is a one-man language (Rich Hickey is the BDFL) and provides
access to all the Java libraries. Moreover it provides a whole set
of `syntax conveniences`_ that would never enter in the Scheme standard.

Professionally I have never
interacted with the Java platform (and even there I would probably
choose Jython over Clojure for reason of familiarity) so I have not
checked out Clojure and I have no idea about it except what you can
infer after reading its web site. If amongst my readers
there is somebody with experience in Clojure, please feel free to add
a comment to this post.

I personally am using Scheme since I am interested in macrology and no
language in existence can beat Scheme in this respect.

.. _Clojure: http://clojure.org/
.. _syntax conveniences: http://clojure.org/special_forms
.. _concurrency: http://clojure.org/concurrent_programming
.. _bikeshed effect: http://en.wikipedia.org/wiki/Bikeshed

Second order macros
-------------------------------------------------------------

There is not upper limit to the level of sophistication you can reach
with macros: in particular it is possible to define higher order
macros, i.e. macros taking other macros as arguments or macros
expanding to other macros. Higher order macros allows an extremely
elegant programming style; on the other hand, they are exposed to the
risk of making the code incomprehensible and very hard to debug.
In this episode we will give a couple of examples of second order
macros taking other macros as argument.

Our first example is a generalization of the accumulator trick we
used last week to define the ``cond-`` macro. We will define a
``collecting-pairs`` macro, which as input another macro and a
sequence of arguments, and calls the input macro with the arguments
grouped in pairs.
Here is the code:

$$COLLECTING-PAIRS

``collecting-pairs`` can be used with many syntactic expressions like
``cond``, ``case``, ``syntax-rules``, et cetera. Here is an example
with the case_ expression::

 > (collecting-pairs (case 1)
       (1) 'one
       (2) 'two
       (3) 'three
       else 'unknown))
 one

Our second example if is a ``:`` macro defined as follows:

$$lang:COLON

The colon macro expects as argument another macro, the
``let-form``, which can be any binding macro such that
``(let-form ((patt value)) expr)`` is a valid syntax. For instance
``(let ((name value)) expr)`` can be rewritten as ``(: let name value
... expr)``, by removing four parenthesis. The latest version of the
``aps`` package provides a colon form in the ``(aps lang)`` module.


.. _case: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_384
..  _Arc: http://www.paulgraham.com/arcll1.html

A two-level syntax
-------------------------------------------------------------

Parens-haters may want to use ``collecting-pairs`` and the colon macro
to avoid parenthesis. They may even go further, and rant that the
basic Scheme syntax should require less parenthesis, since for
most programmers it is easier to write code with less parenthesis.
However, the Scheme philosophy favors automatic code generation
over manual writing. For instance, when writing macros, it is much easier
to use a conditional with more parenthesis like ``cond`` than a
conditional with less parenthesis like ``cond-``. The parenthesis
allows you to group expressions in group that can be repeated via
the ellipsis symbol; in practice, you can writing things like
``(cond (cnd? do-this ...) ...)`` which cannot be written
with ``cond-``.

On the other hand, different languages adopt different philosophies;
for instance Paul Graham's Arc_ uses less parenthesis. This is
possible since it does not provide a macro system based on
pattern matching (which is a big *minus* in my opinion). Is it possible
to have both? A syntax with few parenthesis for writing code manually
and a syntax with many parenthesis for writing macros. The answer is yes:
the price to pay is to double the constructs of the language and to
use a Python-like approach.

Python is a perfect example of language with a two-level syntax: a
simple syntax, limited but able to cover the most common case, and a
fully fledged syntax, giving all the power which is needed, which
however should be used only rarely. The best designed programming
language I know is Python. While not perfect, Python takes full
advantage of the two-level syntax idea. For instance

====================    =================================
Simplified syntax       Full syntax          
====================    =================================
obj.attr                getattr(obj, 'attr')
x + y                   x.__add__(y)
c = C()                 c = C.__new__(C); c.__init__()
====================    =================================

In the case of the conditional syntax, in principle we could have
a fully parenthesised ``__cond__`` syntax for usage in macros and
``cond`` syntax with less parens for manual usage. That, in theory:
in practice Scheme only provides the low level syntax, leaving to
the final user the freedom (and the burden) of implementing his
own preferred high level syntax.

|#

(import (rnrs) (sweet-macros) (for (aps lang) run expand)
        (aps easy-test) (for (aps list-utils) expand) (aps compat))

;;COLLECTING-PAIRS
(def-syntax collecting-pairs
  (syntax-match ()
    (sub (collecting-pairs (name arg ...) x1 x2 ...)
     #'(collecting-pairs "helper" (name arg ...) () x1 x2 ...))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...))
     #'(name arg ... acc ...))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...) x)
     #'(syntax-violation 'name "Mismatched pairs" '(name arg ... acc ... x) 'x))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...) x1 x2 x3 ...)
     #'(collecting-pairs "helper" (name arg ...) (acc ... (x1 x2)) x3 ...))
    ))
;;END

;;TEST-COLON
(run
 (test "ok"
       (: let* x 1 y x (+ x y))
       2)
;  (test "err"
;     (catch-error (: let* x 1 y x z (+ x y)))
;      "Odd number of arguments")
 )

;;END
