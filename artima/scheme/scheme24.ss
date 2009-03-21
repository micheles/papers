#|
The Dark Tower
---------------------------------------------------------------------

Even if strong phase separation could be a bad idea,
the R6RS standards allows it (even if it
does not forbid weak phase separation, nor lack of phase separation)
and the biggest Scheme implementation (PLT) makes use of it, as well
as other well known implementations (such as Larceny) so I am forced
to discuss it in detail. Strong phase separation imposes, as a necessary
consequence, the existence of a (Dark) Tower of import/export levels,
or meta-levels_. Since the publication of the Aristotle's Metaphysics,
the word *meta* has been associated to arcane and difficult matters.
The concept of meta-level is no exception to the rule ;)

You can find a full description in the R6RS document, in a rather dense
paragraph in section 7 of the report. Here instead of repeating the
report, I will try to show some concrete example of macro where it is
essential to understand the meta-levels.

The macro is a compile time ``name -> value`` mapping, with some
introspection feature:

$$experimental/ct-mapping:

This is a kind of second order macro, since it expands
to a macro transformer; its usage is obvious in Ikarus, since it has
only weak phase separation:

$$use-ct-mapping.ikarus:

If you run this script in Ikarus or Ypsilon
you will get the following non-surprising result::

 $ ikarus --r6rs-script use-ct-mapping.ikarus.ss 
 Available colors: (red green yellow)(R G Y)

However, in PLT Scheme, the above will fail::

 $ plt-r6rs use-ct-mapping.ikarus.ss 
 use-ct-mapping.ikarus.ss:3:19: compile: unbound variable in module
 (transformer environment) in: ct-mapping

That could be surprising, since. However, PLT Scheme does not make
special cases for macros: both functions and macros require phase
specification. So you may be tempted to change the import to
``(for (experimental ct-mapping) expand))`` and to run the script
again, but now it will fail with a very cryptic error message::

 $ plt-r6rs use-ct-mapping.mzscheme.ss
 /home/micheles/.plt-scheme/4.0/collects/experimental/ct-mapping.sls:8:25:
 compile: bad syntax; reference to top-level identifier is not allowed,
 because no #%top syntax transformer is bound in: quote

I was baffled by this error, so I asked for help in the PLT mailing
list, and I got this answer by Matthew Flatt:

  When you move to the right-hand side of ``def-syntax``, then the
  relevant binding level is one more than the context of the definition.
  So, if we start from 0, then we move to 1 for the outer ``syntax-match``.
  That should be as expected; you're using ``syntax-match`` in an
  expand-time position.  
  When you go into a template, as in the right-hand side of a
  ``syntax-match`` clause, then you go back down one phase level. So from 1
  for the outer ``syntax-match``, we'd go back down to 0 for the binding of
  the inner ``syntax-rules``. (A ``for ... expand`` import will shift this
  back up to phase level 1.)
  Unusually in this case, the content of the template is itself a
  ``syntax-match``, which has its own templates. We go down one more level
  for the nested template that contains a quote. That is, we go from
  level 0 for the inner ``syntax-match`` to level -1 for the quote. (Again,
  a ``for ... expand`` import shifts it back to phase level 0, which
  corresponds to run-time of the importing module.)
  So, the macro needs a binding for ``quote`` at phase level -1.

.. _meta-levels: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_sec_7.2
|#
