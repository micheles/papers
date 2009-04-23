#|The tower of metalevels
=============================================

This episode goes down in dept through the Dark Secrets and Black Magic
of Scheme implementations with strong phase separation and their
undetermined tower of metalevels.

The Dark Tower
---------------------------------------------------------------------

Even if I do not like full phase separation, the R6RS
standards allows it and the biggest Scheme implementation (PLT) makes
use of it, as well as other well known implementations (such as
Larceny) so I am forced to discuss it in detail.

Strong phase separation is quite complicated and cumbersome, because
it forces you to reason in terms of a (Dark) Tower of import/export levels, or
meta-levels_. Since the publication of the Aristotle's Metaphysics,
the word *meta* has been associated to arcane and difficult matters.
The concept of meta-level is no exception to the rule ;)

.. figure:: DarkTower.jpg

   The dark tower of metalevels

You can find a full description in the R6RS document, in a rather dense
paragraph in section 7 of the report. Here instead of repeating the
report, I will try to show a concrete example of a macro where it is
essential to understand the meta-level concept.

My example macro is a compile time ``name -> value`` mapping, with some
introspection feature:

$$experimental/ct-mapping:

This is a kind of second order macro, since it expands
to a macro transformer; its usage is obvious in Ikarus, since it has
only weak phase separation:

$$use-ct-mapping:

If you run this script in Ikarus or Ypsilon
you will get the following unsurprising result::

 $ ikarus --r6rs-script use-ct-mapping.ikarus.ss 
 Available colors: (red green yellow)(R G Y)

However, in PLT Scheme, the above will fail with a very cryptic error message::

 $ plt-r6rs use-ct-mapping.ss
 /home/micheles/.plt-scheme/4.0/collects/experimental/ct-mapping.sls:8:25:
 compile: bad syntax; reference to top-level identifier is not allowed,
 because no #%top syntax transformer is bound in: quote

I was baffled by this error, so I asked for help in the PLT mailing
list, and I got a very enlightning answer from Matthew Flatt.
The tricky point here is that there is nothing wrong with
the client script, and there is no way to fix the problem by editing
it: the problem is in the library code! However, the problem is silent
(you may compile the library without issues) and
you see it only when you use the client code, not when you compile
the library, this is why I say it is very tricky. Also, the fix is pure
dark magic: you need to rewrite the import
code in ``(experimental ct-mapping)`` by replacing

  ``(import (rnrs))``

with

  ``(import (rnrs) (for (rnrs) (meta -1))``

i.e. the ``ct-mapping`` macro must import the ``(rnrs)`` environment
at metalevel -1! Why it is so? and what the hell is metalevel -1?

More on metalevels
---------------------------------------------------------------------

We have already encountered two metalevels: the runtime (metalevel 0)
and compile time (metalevel 1). Actually the forms ``(for (lib) run)``
and ``(for (lib) expand)`` are just shortcuts for ``(for (lib) (meta
0))`` and ``(for (lib) (meta 1))``, respectively. However, the full
tower of metalevels is arbitrarily high (!) and extends in two directions,
both for positive and for negative integers.

The basic concept of metalevel should be already clear: the right hand
side of a macro can only refer to names which are one metalevel up,
i.e. typically at metalevel 1 (expand time).  On the other hand,
inside a template one goes back one metalevel, and usually a template
expands at metalevel 0 (runtime).

However, in the case of ``ct-mapping`` macro, the
template is itself a ``syntax-match`` form, and therefore the
templates of this innner ``syntax-match`` expand one level down, at
metalevel -1.  This is why the macro needs to import the ``(rnrs)``
bindings at metalevel -1 and why the error message says that ``quote``
is unknown.

Actually ``quote`` is the only needed binding, so it would be enough
import it with the syntax ``(for (only (rnrs) quote) (meta -1))``. If
we ignored the introspection feature, i.e. we commented out the line

``(sub (ctx <names>) #''(name ...))``

there would be no need to import ``quote`` at metalevel -1, and the macro
would work without us even suspecting the existence of negative metalevels.

It is clear that the metalevel tower is theoretically unbound in the
negative direction, since you can nest macro transformers at any level
of depth, and each level decreases the metalevel by one unity; on the
other hand, the tower is theoretically unbound even in the positive direction,
since a macro can have in its right hand side a macro definition which
right hand side will requires bindings defined at an higher level, and
so on. Macro definitions *increase* the metalevel; macro templates
*decrease* the metalevel. However, I suggest you not to think much
about the metalevel tower, if you don't want to risk your head ;)

.. image:: exploding-head.jpg

Things are even trickier: if we keep the line
``(sub (ctx <names>) #''(name ...))`` in the original macro, but we
do not use it in client code, the original macro will apparently work,
and will break at the first attempt of using the introspection
feature, with an error message pointing to the problem in client code,
but not in library code!

All this trouble is missing in Ikarus and Ypsilon, where
importing a module once imports it for *all* metalevels.
In Ikarus and Ypsilon, all metalevels (or phases) share to the same
language and this is why I say they are weakly separated. This remark
should also make clear why many people (including myself) prefer
weak phase separation to strong phase separation.

.. _meta-levels: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_sec_7.2

A note about identitifier macros
--------------------------------------------------------------

I have always hated being forced to put my helper functions in an
auxiliary module, because I often use auxiliary functions which
are intended to be used only once inside a given macro, thus
it makes sense to put those auxiliary functions *in the same
module* as the macro the are used in.
In principle you could solve the problem by definining all the
functions *inside* the macro, but I hate this, both for dogmatic
reasons (it is a Pythonista dogma that *flat is better than nested*)
and for pragmatic reasons, i.e. I want to be able to debug my
helper functions and this is impossible if they are hidden inside
the macro. They must be available at the top-level. Moreover, you
never know, and a functionality which was intended for use in a specific
macro my turn useful for another macro after all, and it is much
more convenient if the functionality is already encapsulated in
a nice exportable top level function.

I am not the only to believe that it should be possible to define
helper functions in the same module as the macro and actually
many Scheme implementations provide a way to do so via a
``define-for-syntax`` form which allows to define function
at *expand time*, so that they are available for usage in macros.

If your Scheme does not provide ``define-for-syntax``, which is not
part of the R6RS specification, you can still work around phase
separation with some clever hack. For instance, you could
use the following macro:

$$lang:LITERAL-REPLACE

|#
