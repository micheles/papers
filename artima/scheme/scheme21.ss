#|Phase separation
===================================================================

The Scheme module system is complex, because of the
complications caused by macros and because of the want of
separate compilation and cross compilation.
However, fortunately, the complication
is hidden, and the module system works well enough for many
simple cases. The proof is that we introduced the R6RS module
system in episode 5_, and for 20 episode we could go on safely
by just using the basic import/export syntax. However, once
nontrivial macros enters in the game, things start to become
interesting.

.. _5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

We saw in the last episode that Scheme programs executed
in compiler semantics exhibit phase separation, i.e. some code
is executed at compile (expand) time and some code is executed
at runtime. Things, however, are more complicated than that.


Phase specification
--------------------------------------------------------------

Let me go back to the example of the ``assert-distinct`` macro.
I have put the ``distinct?`` helper function in the ``(aps list-utils)``
module, so that you can import it.  This is enough to solve the
problem of compile time vs runtime separation for Ikarus, but it is not
enough for PLT Scheme or Larceny, which require *phase specification*.
In other words, in Ikarus (but also Ypsilon, IronScheme and Mosh)
the following script

$$assert-distinct:

is correct, but in PLT Scheme and Larceny it raises an error::

 $ plt-r6rs assert-distinct.ss 
 assert-distinct.ss:5:3: compile: unbound variable in module
 (transformer environment) in: distinct?

.. image:: salvador-dali-clock.jpg

The problem is that PLT Scheme requires *phase specification*: by default
names defined in external modules are imported *only* at runtime, *not*
at compile time. In some sense this is absurd since
names defined in an external pre-compiled modules
are of course known at compile time
(this is why Ikarus has no trouble to import them at compile time);
nevertheless PLT Scheme (and Larceny) forces you to specify at
which phase the functions must be imported.  Notice that personally I
do not like the PLT and Larceny semantics since it makes things more
complicated, and that I prefer the Ikarus semantics:
nevertheless, if you want to write
portable code, you must use the PLT/Larceny semantics, which is the
one blessed by the R6RS document.

If you want to import a few auxiliary functions
at expansion time (the time when macros are processed; often
incorrectly used as synonymous for compilation time) you must
use the ``(for expand)`` form:

``(import (for (only (aps list-utils) distinct?) expand))``

With this import form, the script is portable in all R6RS implementations.

Strong vs week phase separation
-----------------------------------------------------------

There are two different concepts of phase
separation, even for R6RS-conforming implementations.
Ikarus, Ypsilon, IronScheme e MoshScheme have
a weak (partial) form of phase separation: there is a distinction
between expand time and runtime, but it is not possible to import
names only at runtime or only at expand time, i.e. the phases are
not fully separated. PLT Scheme and Larceny instead have a strong
form of phase separation in which phases are completely separated:
in such implementations, when you import (more correctly
*instantiate*) a module, you may specify in which phases to import
it, and each phases sees a *different instance* of the module.
The consequence is that the language used at at compile time (the
language seen by a piece of code is the sum of the imported names)
can be completely different from the language used at runtime. In particular
you could decide to use in macros a subset of the full R6RS language.

Suppose for instance you are a teacher, and you want to force your
students to write their macros using only a functional subset of Scheme.
You could then import at compile time all R6RS procedures except the
nonfunctional ones (like ``set!``) while importing at runtime
the whole R6RS. You could even perform the opposite, and remove ``set!``
from the runtime, but allowing it at compile time.

Therefore strong phase separation is strictly *more powerful* than week
phase separation, since it gives you more control. In implementations
with weak/partial phase separation when
you import a name in your module, the name is imported in all phases,
and there is nothing you can do about it. For instance this program
in Ikarus (but also IronScheme, Ypsilon, MoshScheme)

.. code-block:: scheme

 (import (rnrs) (for (only (aps list-utils) distinct?) expand))
 (display distinct?)

runs, contrarily to what one would expect, because it is impossible
to import the name ``distinct?`` at expand time and not at runtime.
In PLT Scheme and Larceny instead the program will not run, as you
would expect.

You may think the R6RS document to be schizophrenic, since it
accepts both implementations with phase separation and without
phase separation. The previous program is *conforming* R6RS code, but
behaves *differently* in R6RS-compliant implementations!

A precise specification of the library system
   remains elusive, partly because different
   implementors still have different ideas about how
   the library system should work....

   The different opinions are supported by two
   different reference implementations of R6RS
   libraries: one by Van Tonder and one by Ghuloum and
   Dybvig.  In addition, PLT Scheme implements a
   library system...

   Despite the differences in the reference
   implementations, it appears that many programs will
   run the same in both variants of the library system.
   The overlap appears to be large enough to support
   practical portability between the variants.

   Under the assumption that the overlap is useful, and
   given the lack of consensus and relative lack of
   experience with the two prominent variants of draft
   R6RS libraries, the R6RS specification of libraries
   should be designed to admit both of the reference
   implementations.  As a design process, this
   implementation-driven approach leaves something to
   be desired, but it seems to be the surest way forward.

.. rationale: http://www.r6rs.org/formal-comments/comment-92.txt


but using the semantics without phase separation results in
non-portable code. Here a bold decision was required to ensure
portability: to declare the PLT semantics as the only acceptable one,
or to declare the Dibvig-Gouloum semantics as the only acceptable one. 

De facto, the R6RS document is the result
of a compromise between the partisans of phase separation
and absence of phase separation.


There is still the question if strong phase separation is a good thing,
or if weak phase separation (as in Ikarus) is enough. For the programmer
weak phase separation is easier, since he does not need to specify
the phase in which he wants to import names.
On the other hand strong phase separation makes everything more complicated:
it is somewhat akin to the introduction of multiple namespace, because
the same name can be imported in a given phase and not in another,
and that can lead to confusion. To contain the confusion, the R6RS
documents states that *the same name cannot be used in different phases
with different meaning in the same module*.
For instance, if the identifier ``x`` is bound to the
value ``v`` at the compilation time and ``x`` is defined even
at runtime, ``x`` must be bound to ``v`` even at runtime. However, it
is possible to have ``x`` bound at runtime and not at compile time, or
viceversa. This is a compromise, since PLT Scheme in non R6RS-compliant mode
can use different bindings for the same name at different phases.

There are people in the Scheme community thinking that strong phase
separation is a mistake, and that weak phase separation is the right thing
to do. On the other side people (especially from the PLT community where
all this originated) sing the virtues of strong phase separation and say
all good things about it. I personally I have not seen a compelling
use case for strong phase separation yet.
On the other hand, I am well known for preferring simplicity over
(unneeded) power. 

.. _5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

More examples of macros depending on helper functions
-----------------------------------------------------------------

In the previous episode I have shown an example of a macro
(``assert-distinct``) depending by a helper function (``distinct?``)
appearing in a guarded pattern. This is not the only example of
macros depending on an external function; actually, external
functions appears more commonly in conjunction with the R6RS standard
form ``with-syntax``, which
allows to introduce auxiliary pattern variables
(a better name would have been ``let-pattern-vars``).
Here is a simple example of usage of ``with-syntax`` in conjunction
with the ``range`` function we introduced in episode 5_, to define
a compile-time indexer macro:

$$INDEXER-SYNTAX

Since both ``range`` and ``distinct?`` are defined in the list-utils
module and used at compile-time, everything works if you use
the import form ``(for (aps list-utils) expand)``.

You can understand how the macro works by expanding it; for instance
``(indexer-syntax a b c)`` expands into a macro transformer that
associates an index from 0 to 2 to each literal identifier from
``a`` to ``c``:

.. code-block:: scheme

 > (syntax-expand (indexer-syntax a b c))
 (syntax-match (a b c)
   (sub (ctx a) #'0)
   (sub (ctx b) #'1)
   (sub (ctx c) #'2))

The ``with-syntax`` form introduces the list of pattern variables ``(i ...)``
corresponding to the list ``(0 1 2)`` generated by the ``range`` function
by looking at the number of arguments, the ``(name ...)`` list.
The guarded pattern also checks that the names are distinct.
Thus, the following test passes:

$$TEST-INDEXER-SYNTAX

The basic feature of the indexer is that ``i`` is a macro and therefore literal
identifiers are turned into integers at compile time, *without runtime
penalty*. On the other hand, if you want to turn symbols known only
at runtime into integers, the previous approach does not work, and
you can define a runtime indexer as follows:

$$INDEXER

Here the indexer is a simple function; the acceptable symbols
are specified (and checked) at expand time, but the dispatch
is performed at runtime. Here is a test:

$$TEST-INDEXER

You may enjoy yourself with performance benchmarks comparing the macro
solution with the function solution; moreover you
can contrast this solution with the builtin way of R6RS Scheme
to build indexers as sets.

The problem with auxiliary macros
------------------------------------------------------------------

We said a few times that auxiliary functions are not available to macros
defined in the same module, but in general
there is the *same* problem for any identifier which is used in the
right hand side of a macro definition, *including auxiliary macros*.

In systems with strong phase separation, like
PLT Scheme and Larceny, auxiliary macros
are not special, and they behave as auxiliary functions: you
must put them into a separare module and you must import them
with ``(for (only (module) helper-macro) expand)`` before using them.

In particular, the ``indexer-syntax`` macro defined before is
an auxiliary macro, to be used in the right hand side of a
``def-syntax`` form. The following script

$$indexer-syntax:

fails in PLT scheme

::

 $ plt-r6rs indexer-syntax.ss
 indexer-syntax.ss:9:0: def-syntax: bad syntax in: (def-syntax (indexer-syntax a b c))

  === context ===
 /usr/lib/plt/collects/rnrs/base-6.ss:492:6


because the second ``def-syntax`` does
not see the binding for the ``indexer-syntax`` macro.

This is a precise design choice: systems with full phase
separation are making the life harder for programmers,
by forcing them to put auxiliary functions/macros/objects
in auxiliary modules, to keep absolute control on how the
names enter in the different phases and to make possible
to use different languages at different phases.

I have yet to see a convincing example of why keeping
different languages at different phases is worth
the annoyance.

On the other hand, in systems with weak phase separation,
like Ikarus/IronScheme/Mosh/Ypsilon,
*there is no need to put auxiliary
macros in an external module.* The reason is that all macro
definitions are read at the same time, and the compiler knows
about the helper macros, so it can use them. The consequence
is that a long as the compiler reads macro definitions, it expands
the compile-time namespace of recognized names which are available
to successive syntax definitions.

In such systems the script ``indexer-syntax.ss`` is
perfectly valid: the first syntax
definition would add a binding for ``identifier-syntax`` to the macro
namespace, so that it would be seen by the second syntax definition.

In any case, if you want to write portable code, you must follow
the PLT/Larceny route, to put your auxiliary macros in a separated
module and to import them at expand time.
|#
            
(import (rnrs) (sweet-macros) (for (aps list-utils) expand run)
        (for (aps lang) expand) (aps compat) (aps easy-test))

;;INDEXER-SYNTAX
(def-syntax (indexer-syntax name ...)
  (with-syntax (((i ...) (range (length #'(name ...)))))
    #'(syntax-match (name ...) (sub (ctx name) #'i) ...))
  (distinct? free-identifier=? #'(name ...))
  (syntax-violation 'assert-distinct "Duplicate name" #'(name ...)))
;;END


;;INDEXER
(def-syntax (indexer name ...)
  (with-syntax (((i ...) (range (length #'(name ...)))))
    #'(lambda (x) (case x ((name) i) ...)))
  (distinct? free-identifier=? #'(name ...))
  (syntax-violation 'assert-distinct "Duplicate name" #'(name ...)))
;;END

(display (syntax-expand (indexer-syntax a b c))) (newline)

(run

;;TEST-INDEXER-SYNTAX
(test "indexer-syntax"
      (let ()
        (def-syntax i (indexer-syntax a b c))
        (list (i a) (i b) (i c)))
      '(0 1 2))
;;END

;;TEST-INDEXER
(test "indexer"
      (let ()
        (define i (indexer a b c))
        (list (i 'a) (i 'b) (i 'c)))
      '(0 1 2))
;;END
)
