#|The Dark Tower of Meta-levels
=============================================

.. _You want it when: http://www.cs.utah.edu/plt/publications/macromod.pdf
.. _R6RS document: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_sec_7.2

I said in the previous episode that even if your implementation of choice does
not use explicit phasing, you must understand it in order to write
portable programs.  Truly understandanding explicit phasing is nontrivial,
since you must reason in terms of a (Dark) Tower of import levels, or
*meta-levels*.

Since the publication of the Aristotle's Metaphysics,
the word *meta* has been associated to arcane and difficult matters.
The concept of meta-level is no exception to the rule.
You can find a full description of the tower of meta-levels in the
`R6RS document`_, in a rather dense paragraph in section 7 that will make
your head hurt.

There is also a
celebrated paper by Matthew Flatt, *Composable and Compilable
Macros* (a.k.a. `You want it when`_) which predates the R6RS by many
years and is more approachable. Its intent is to motivate
the module system used by PLT Scheme, which made popular
the concept of tower of meta-levels.

Meta-levels are just another name for phases.
We have already encountered two meta-levels: the
run-time phase (meta-level 0) and expand time phase (meta-level 1).
However, the full tower of meta-levels is arbitrarily
high and extends in two directions, both for positive and for
negative integers (!)

.. figure:: DarkTower.jpg

   Aziz faces the Dark Tower of Meta-levels

Scheme implementations with explicit phasing allow you to
import a module at a generic meta-level ``N`` with the syntax
``(import (for (lib) (meta N)))``, where N is an integer.
The forms ``(import (for (lib) run))`` and ``(import (for (lib) expand))`` are just
shortcuts for ``(import (for (lib) (meta 0)))`` and
``(import (for (lib) (meta 1)))``, respectively.

Instead of discussing much theory, in this episode I will
show two concrete examples of macros which require
importing variables at a nontrivial meta-level *N*,
with *N<0* or *N>1*.

For convenience I am keeping all the code of this episode into a
package called ``experimental``, which you can download
from here: http://www.phyast.pitt.edu/~micheles/scheme/experimental.zip

An easy-looking macro with a deep portability issue
------------------------------------------------------

My first example is a compile time ``name -> value`` mapping, with some
introspection:

$$experimental/static-map:

This is a kind of second order macro, since it
expands to a macro transformer; its usage is obvious in
implementations with implicit phasing::

 $ cat use-static-map.ss

$$use-static-map:

``color`` is a macro which replaces a symbolic name
in the set ``red``, ``green``, ``yellow`` with its
character representation (``#\R``, ``#\G``, ``#\Y``)
at expand-time (notice that in Scheme characters
are different from strings, i.e. the character
``\#R`` is different from the string of length 1 ``"R"``).

If you run this script in Ikarus or Ypsilon or Mosh
you will get the following unsurprising result::

 $ ikarus --r6rs-script use-static-map.ikarus.ss 
 Available colors: (red green yellow)(R G Y)

However, in PLT and Larceny, the above will fail. The PLT error message
is particularly cryptic::

 $ plt-r6rs use-static-map.ss
 /home/micheles/.plt-scheme/4.0/collects/experimental/static-map.sls:8:25:
 compile: bad syntax; reference to top-level identifier is not allowed,
 because no #%top syntax transformer is bound in: quote

I was baffled by this error, so I asked for help in the PLT mailing
list, and I discovered that there is nothing wrong with the client
script and that there is no way to fix the problem by editing it: the
problem is in the library code!

The problem is hidden, since you can compile the library without issues and
you see it only when you use it. Also, the fix is pure
dark magic: you need to rewrite the import
code in ``(experimental static-map)`` by replacing

  ``(import (rnrs))``

with

  ``(import (rnrs) (for (rnrs) (meta -1))``

i.e. the ``static-map`` macro must import the ``(rnrs)`` environment
at meta-level -1! Why it is so? and how should I interpret meta-level -1?

Negative meta-levels
---------------------------------------------------------------------

Matthew Flatt explained to me how meta-levels work.
The concept of meta-level is only relevant in macro programming.  When
you define a macro, the right hand side of the definition can only
refer to names which are one meta-level up, i.e. typically at meta-level
1 (expand time).  On the other hand, inside a template one goes back
one meta-level, and therefore usually a template expands at meta-level
0 (run-time).

However, in the case of the ``static-map`` macro, the
template is itself a ``syntax-match`` form, and since the
templates of this inner ``syntax-match`` expand one level down, we
reach meta-level -1.  This is why the macro needs to import the ``(rnrs)``
bindings at meta-level -1 and why the error message says that ``quote``
is unknown. The comments below should make clear how meta-levels mix:

.. code-block:: scheme

 (def-syntax static-map                          ;; meta-level 0
  (begin
    <there could be code here ...>               ;; meta-level 1
  (syntax-match ()
   (sub (static-map (name value) ...)
        #'(begin
            <there could be code here ...>       ;;  meta-level 0
            (syntax-match (<names> name ...)
              (sub (ctx <names>)
                 #''(name ...))                  ;; meta-level -1
              (sub (ctx name)
                 #'value)                        ;; meta-level -1
              ...))))

Actually ``quote`` is the only needed binding, so it would be enough
to import it with the syntax ``(import (for (only (rnrs) quote) (meta -1)))``.
If we ignored the introspection feature, i.e. we commented out the line

``(sub (ctx <names>) #''(name ...))``

there would be no need to import ``quote`` at meta-level -1, and the macro
would work without us even suspecting the existence of negative meta-levels.

Things are even trickier: if we keep the line
``(sub (ctx <names>) #''(name ...))`` in the original macro, but we
do not use it in client code, the original macro will apparently work,
and will break at the first attempt of using the introspection
feature, with an error message pointing to the problem in client code,
but not in library code :-(

Meta-levels greater than one
------------------------------------------------------------

It is clear that the meta-level tower is theoretically unbound in the
negative direction, since you can nest macro transformers at any level
of depth, and each level decreases the meta-level by one unity; on the
other hand, the tower is theoretically unbound even in the positive direction,
since a macro can have in its right hand side a macro definition which
right hand side will requires bindings defined at an higher level, and
so on.  In general nested macro definitions *increase* the meta-level;
nested macro templates *decrease* the meta-level.

Here is an example of a macro which requires importing names at meta-level 2::

 $ cat meta2.ss

$$meta2:

Notice that right hand side of a ``def-syntax`` form does not need to
be ``syntax-match`` form; the only requirement for it is to be a
transformer, i.e. a one-argument procedure. In this example the
inner macro ``m2`` has a transformer returning the string ``"m-expanded"``
whereas the outer macro ``m`` has a transformer returning the expansion
of ``(m2)`` i.e. again the string
``"m-expanded"``. Running the script will print the following::

 $ ikarus --r6rs-script meta2.ss
 at meta-level 2
 at meta-level 1
 expanded-m

You will get the same in Larceny and in sufficiently recent versions
of PLT Scheme (> 4.1.3). Currently Ypsilon raises an exception but this is
just a bug (`already fixed in the trunk`_).

.. _already fixed in the trunk: http://code.google.com/p/ypsilon/issues/detail?id=98

Discussion
------------------------------------------

The concept of meta-level is tricky. On one hand, there only two physical
meta-levels, i.e. the run-time (when the code is executed) and the compile
time (when the code is compiled). On the other hand, conceptually there is
an arbitrary number of positive meta-levels ("before compile time") and
negative meta-levels ("after run-time") which have to be taken in
account to compile/execute a program correctly: everytime the compiler
look at a nested macro, it has to consider the innermost level first,
and the outermost level last.

The power (and the complication) of phase specification is that the
language used at a given phase can be different from the language used
in the other phases.
Suppose for instance you are a teacher, and you want to force your
students to write their macros using only a functional subset of Scheme.
You can do so by importing at compile time all R6RS procedures except the
nonfunctional ones (like ``set!``) while importing at run-time
the whole of R6RS. You could even perform the opposite, and remove ``set!``
from the run-time, but allowing it at compile time.

However, personally I do not feel a need to distinguish the languages
at different phases and I like Scheme to be a Lisp-1 language with a
single namespace for all variables.  I am also not happy with having
to keep manually track of the meta-levels, which is difficult and
error prone when writing higher order macros. Moreover, in PLT and
Larceny writing a macro which expands to a nested macro with N levels
is difficult, since one has to write by hand all the required meta imports.

.. figure:: tower_of_babel.jpg

   Aziz destroys the Tower of Meta-levels

All this trouble is missing in Ypsilon and in the
implementations based on psyntax. In such
systems importing a module imports its public variables for *all* meta-levels.
In other words all meta-levels share the same language: the
tower of meta-levels is effectively destroyed (one could
argue that the tower is still there, implicitly, but the point is that
the programmer does not need to think about it explicitly). The model
of implicit phasing was proposed by Kent Dybvig and
Abdul Aziz Ghuloum, who wrote his `Ph. D. thesis`_ on the subject.

.. _Ph. D. thesis: http://portal.acm.org/citation.cfm?id=1291151.1291197&coll=GUIDE&dl=GUIDE&CFID=34012650&CFTOKEN=38507862
|#
