#|Macros taking macros as arguments
============================================================

Our journey though the macro programmer toolkit continues.
In this episode I will give a couple of examples of second order
macros taking other macros as argument. Moreover I will bring
an argument in favor of good old parenthesis.

Scheme as an unfinished language
-----------------------------------------------------------------------

Python is an example of a *finished* language. With *finished*, I mean
that the language has not only a well defined and complete core, but also
a full toolbox of shortcuts and niceties to take care of common
cases and to make easier the life of the programmer. In other words
Python (but I could name Ruby or a few other languages here) is an
example of a language which is both easy to use and powerful. It
spoils the programmer, and this is the reason it is so much popular
nowadays. Of course, others will prefer Ruby, or Scala, or
something else, but the concept of finished language should be
clear.

On the other hand, Scheme feels very much like an unfinished language
from somebody coming from the Python world. While there are many
practical reasons why Scheme it is the way it is and could not be
different, I am convinced that Scheme has been left unfinished also
*on purpose*: the language is incomplete, but it provides a built-in
mechanism to give the user the ability to finish the language
according to its preferences. Such a mechanism of course is the
mechanism of macros and one of the main use of macros is to fill
the deficiencies left by the standard.

.. image:: bikeshed.jpg
 :class: right
 :width: 400

I think the explanation for the current situation in Scheme is more
historical and social than technical. On one side, a lot of people in
the Scheme world want Scheme to stay the way it is, i.e. a language
for language experimentations and research more than a language for
enterprise work.  On the other side, the fact that there are so many
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

The difference between Scheme and its concrete implementations
--------------------------------------------------------------------

In the previous paragraph I have been a little unfair: while it is
true that Scheme - in the sense of the language specified by the
R6RS standard - is unfinished, concrete implementations of Scheme
tends to be much more complete. Consider for instance PLT Scheme,
or Chicken Scheme, which are two big Scheme implementations: they
have both decent size libraries and they are perfectly usable
(and used) for practical tasks you could do with Python or another
more mainstream language. Another option is to use a Scheme
implementation running on the Java virtual machine or on the .NET
platform. Alternatively, you could use a Scheme-like
language such as Clojure_. Clojure runs on the Java Virtual Machine,
it is half lisp and half Scheme, it has a strong functional flavour in
it, it has interesting things to say about concurrency_. It also
shares the following caracteristics with Python:

1. it is a one-man language (i.e. it is not a comprimise language made
   by a committee) with a clear philosophy and internal consistency;

2. it is language made from scratch, with no preoccupations of backward
   compatibility;

3. it provides special syntax/libraries for common operations (
   `syntax conveniences`_) that would never enter in the Scheme standard.

Such caracteristics make Clojure very appealing to me. The only
problems is that professionally I have no need to
interact with the Java platform (and even there I would probably
choose Jython over Clojure for reason of familiarity) so I have not
checked out Clojure and I have no idea about it except what you can
infer after reading its web site. If amongst my readers
there is somebody with experience in Clojure, please feel free to add
a comment to this post.

My point here is that users that do not care at all for the freedom
to "finish" their language and prefer a BDFL language where
everything has been already finished and polished for them have
that option, by choosing Clojure.

I personally am using Scheme since I am interested in macrology and no
language in existence can beat Scheme in this respect. Also, I am
using for Scheme for idle speculation and not to get anything done ;-)
A typical example of idle speculation is
the following question: would Scheme be a better language if it has
fewer parenthesis?

.. _Clojure: http://clojure.org/
.. _syntax conveniences: http://clojure.org/special_forms
.. _concurrency: http://clojure.org/concurrent_programming
.. _bikeshed effect: http://en.wikipedia.org/wiki/Bikeshed


Two second order macros to reduce parenthesis
-------------------------------------------------------------

While finding tricks for reducing parenthesis is pointless,
it gives me a reason to teach a few other macro programming
techniques: in particular, here I will discuss second order
macros taking macros as arguments.

In the last episode I defined a recursive ``cond-`` macro taking
less parenthesis than a regular ``cond``, using an accumulator.  Here
I will generalize that approach, by abstracting the accumulator
functionality into a second order macro, called ``collecting-pairs``,
which takes as input another macro and a sequence of arguments, and
calls the input macro with the arguments grouped in pairs. 
That makes it possible to call with less parenthesis any macro of
the form ``(macro expr ... (a b) ...)``, by calling it as
``(collecting-pairs (macro expr ...) a b ...)``.

Here is the code implementing ``collecting-pairs``:

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

.. _case: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_384

Consider the "colon" macro defined as follows:

$$lang:COLON

The colon macro expects as argument another macro, the
``let-form``, which can be any binding macro such that
``(let-form ((patt value) ...) expr)`` is a valid syntax. For instance
``(let ((name value) ...) expr)`` can be rewritten as ``(: let name value
... expr)``, by removing four parenthesis. The latest version of the
``aps`` package provides a colon form in the ``(aps lang)`` module.

The case for parenthesis
-------------------------------------------------------------

Parens-haters may want to use ``collecting-pairs`` and the colon macro
to avoid parenthesis. They may even go further, and rant that the
basic Scheme syntax should require less parenthesis, since for
most programmers it is easier to write code with less parenthesis.
However, that would be against the Scheme philosophy:
the Scheme philosophy favors automatic code generation
over manual writing.

When writing macros, it is much easier
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
and a syntax with many parenthesis for writing macros?

The answer is yes:
the price to pay is to double the constructs of the language and to
use a Python-like approach.

..  _Arc: http://www.paulgraham.com/arcll1.html

A two-level syntax
---------------------------------

Python is a good example of language with a two-level syntax: a
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

When it comes to designing programming languages, easy of use and
power seems to go in opposite directions. There are plenty of examples
where something went wrong, i.e. simple languages which are
good only for teaching and not for professional use, and
professional languages which are too tricky to use
for the casual programmer. We have also examples of languages which
are both weak in power *and* difficult to use (insert your chosen
language here).
|#

(import (rnrs) (sweet-macros) (for (aps lang) run expand)
        (aps easy-test) (for (aps list-utils) run expand) (aps compat))

;;DEF-VECTOR-TYPE
(def-syntax (def-vector-type name (field-name checker?) ...)
  (with-syntax (((i ...) (range (length #'(field-name ...)))))
    #'(begin
        (define (check-all vec)
          (vector-map
           (lambda (check? field arg)
             (if (check? arg) arg (error 'name "TypeError" field arg)))
           (vector checker? ...) (vector 'field-name ...) vec))
        (def-syntax name
          (syntax-match (check <name> fields new ref set! field-name ...)
            (sub (ctx check vec) #'(check-all vec))
            (sub (ctx <name>) #''name)
            (sub (ctx fields) #'(list 'field-name ...))
            (sub (ctx from-list ls) #'(check-all (list->vector ls)))
            (sub (ctx new arg (... ...)) #'(ctx from-list (list arg (... ...))))
            (sub (ctx v ref field-name) #'(vector-ref v i)) ...
            (sub (ctx v set! field-name x) #'(vector-set! v i x)) ...
          ))))
  (distinct? free-identifier=? #'(field-name ...)))
;;END

;;BOOK
(def-vector-type Book (title string?) (author string?))
;;END

(display (Book <name>)) (newline)

(pretty-print (syntax-expand
               (def-vector-type Book (title string?) (author string?))))

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

 (test "nv1"
       (let ()
         (define b (Book new "T" "A"))
         (Book b ref title))
       "T")
 )

;;END
