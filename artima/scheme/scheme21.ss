#|Phase separation
===================================================================

We saw in the latest episode that Scheme programs
exhibit phase separation, i.e. some parts of the program are
executed at expand time (import declarations, macro definitions and
macro expansions) and some other parts are executed at runtime
(regular definitions and expressions).

However, things are more complicated than that.  There are actually
*three* different concepts of phase separation for R6RS-conforming
implementations. I will call the three concepts *weak*, *strong* and
*extra-strong* phase separation respectively.  The difference is in
how modules are imported - *instantiated* is the more correct term -
and in how names enter in the namespace.

Ikarus, Ypsilon, IronScheme and MoshScheme have a weak form of phase
separation (also called the implicit phasing model): there is a
distinction between expand-time and runtime, but it is not possible to
import names in the runtime phase only or in the expand time phase
only: names are imported simultaneously for all phases.

Larceny has a stronger form of phase separation: it can import names
in a specific phase on not in the other, depending on the import
syntax used.  However, if you instantiate a module in more than one
phase - for instance both at run-time and at expand-time - only one
instance of the module is created.

PLT Scheme has an extra-strong form of phase separation in which
phases are completely separated: if you instantiate a module both at
run-time and at expand-time, there are two *different and independent
instances* of the module.

In this episode I will show the simplest consequences
of phase separation. In the next episodes I will show less obvious
consequences, such as the tower of metalevels associated
to strong phase separation and the multiple instantiation semantics
associated to extra-strong phase separation.

Compile-time, run-time and optimization-time
---------------------------------------------------------------------

Before discussing strong phase separation, I want to point out that
phase separation, even in its weakest form, has consequences that may be
surprising at first. For instance, Scheme compilers 
(but also the Python compiler) cannot recognize obvious errors like
the zero division error in the right hand side of a top level
definition I have shown in episode 19_.

I asked for clarifications on the Ikarus mailing list. It turns out
that Scheme compilers are not stupid: they can recognize the zero
division error, but they cannot signal it since it is forbidden by the
Scheme specification. For instance, Llewellyn Pritchard (Leppie), the
implementor of IronScheme wrote:

.. epigraph::

 In IronScheme, if I can detect there is an issue at compile
 time, I simply defer the computation to the runtime, or could even
 just convert it into a closure that will return an error. This is only
 one of the things that make Scheme quite hard to implement on a statically
 typed runtime such as the CLR, as it forces me to box values at method
 boundries and plenty type checking at runtime.

whereas Abdul Aziz Ghuloum wrote:

.. epigraph::

 Actually, Ikarus does some type checking, and it does
 detect the division by 0.  It however cannot do anything
 about it in this case since Scheme requires that the
 exception be raised when the division operation is
 performed at run time.

Aziz went further and explained that Ikarus is able to evaluate
expressions like

.. code-block

   (define x 5)
   (define y (+ x 1))
   (define z (* x y)) 

both in top level definitions in and internal definitions; however, it
does so in the optimization phase, i.e. *after* the expansion phase,
i.e. too late to make the definitions available to macros.

Nevertheless, I think that reporting a syntax warning would be a reasonable
idea.

Aziz also brought up an argument in favor of the current
specification. First of all, it is pretty clear that we want
expressions like

.. code-block:: scheme

 (define thunk (lambda () (/ 1 0)))

to be compilable, because it is useful to have functions that can
raise predictable errors, especially when writing test cases.

Now, a module is not really different from a giant thunk; importing a
module calls the thunk (this is essentially what *module instantiation* is)
and possibly raises errors at runtime, but the module per se must be
compilable even if contains errors which are detectable at compile
time.

There are strong arguments against having the compiler evaluating
generic top level or internal definitions; consider for instance the
case when you are reading some data from standard input (``(define
date (read)``): if the definition were evaluated at compile-time, the
compiler would stop during compilation to read the data. Then, some
time later, at execution time, the program would stop again to read
potentially different data, so that macros would use the compilation
time data and the rest of the program the runtime data! That would be
madness. Clearly it makes no sense to evaluate at compile-time definitions
depending on run-time values, except possibly at the REPL, where everything
happens at run-time and the phases are intermingled.

The two-phases compilation strategy has the advantage of keeping the
compiler conceptually simple, working as a traditional preprocessor
integrated in the language: we know that the compiler will manage the
macros, but will not perform any evaluation.

Finally, the two-phases enable `cross compilation`_: macros will be expanded
independently from the architecture, whereas the
runtime structures will be compiled and linked differently depending on the
architecture of the target processor.

.. image:: compiler-crosscompiler.jpg

.. cross compilation: http://chicken.wiki.br/cross-compilation
.. _cross compilation: http://en.wikipedia.org/wiki/Cross_compilation
.. _19: http://www.artima.com/weblogs/viewpost.jsp?thread=251476

Strong phase separation
---------------------------------------------------------------------

.. _formal comment 92: http://www.r6rs.org/formal-comments/comment-92.txt
.. _psyntax: http://ikarus-scheme.org/r6rs-libraries/index.html
.. _20: http://www.artima.com/weblogs/viewpost.jsp?thread=255303

To explain the practical difference between strong and weak phase
separation let me go back to the example of the ``assert-distinct`` macro of
episode 20_.  I have put the helper function (``distinct?``) in the
``(aps list-utils)`` module, so that you can import it.  This is
enough for Ikarus, but it is not enough for PLT Scheme or Larceny.  In other
words, in Ikarus (but also IronScheme, MoshScheme and all the
systems using the psyntax_ module system) the following script

$$assert-distinct:

is correct, since the import form instantiates the module
``(aps list-utils)`` both at run-time and expand-time,
but in PLT Scheme and Larceny it raises an error::

 $ plt-r6rs assert-distinct.ss 
 assert-distinct.ss:5:3: compile: unbound variable in module
 (transformer environment) in: distinct?

The problem is that PLT Scheme and Larceny have strong phase
separation and require *phase specification*: by default names defined
in external modules are imported *only* at runtime, *not* at compile
time. In some sense this is absurd since names defined in an external
pre-compiled modules are of course known at compile time (this is why
Ikarus has no trouble importing them); nevertheless PLT Scheme (and
Larceny) forces you to specify at which phase the functions must be
imported.

In particular, if you want to import ``distinct?`` at expand time you
must use the ``(for expand)`` form:

``(import (for (only (aps list-utils) distinct?) expand))``

With this import form, the script is portable in all R6RS implementations,
but its meaning is different: in the psyntax_ based implementations the
name ``distinct?`` is imported both at runtime and at expand-time,
whereas in PLT and Larceny it is imported only at expand time.

In systems based on psyntax_ and in Ypsilon -
which is not based on psyntax but still has implicit phasing and can
be considered in the same class of implementations - this program

.. code-block:: scheme

 (import (rnrs) (for (only (aps list-utils) distinct?) expand))
 (display distinct?)

runs, but in PLT Scheme and Larceny it will not even compile.

In implementations with implicit phasing it is *impossible* to import
the name ``distinct?`` at expand time and not at runtime, thus
implementation with strong phase separation are somewhat more powerful
than implementations with weak phase separation.

More powerful does not mean better. For instance, implementations with
weak phase separation are easier to use, since you do not need to
specify the import phase.

Unfortunately, not using the phase specification syntax results in
non-portable code, therefore *de facto* if you care about portability
you must understand strong phase separation even if your
implementation does not use it :-(

The situation for people coming from implementations with strong
phase separation is no better. For instance the program

.. code-block:: scheme

 (import (rnrs) (for (only (aps list-utils) distinct?) run))
 (display distinct?)

will run on all implementations, but you cannot rely on the fact
that the named ``distinct?`` will be imported only at run-time
and not at expand-time.

.. image:: salvador-dali-clock.jpg

A note about politics
-----------------------------------------------------------

The reason why such inconsistencies exist can be inferred from
this extract from R6RS editors mailing list (from the answer to
`formal comment 92`_):

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
   
Basically, the R6RS standard is the result of a compromise between the
partisans of strong phase separation - people wanting to control in
which phases names are imported - and the partisan of weak phase
separation - people wanting to import names at all phases, always.

A compromise was reached to make unhappy both parties.

The same kind of compromise was reached on the subject of multiple
instantiation: all behaviors are accepted by the R6RS standard, so
you cannot rely on the number the times a library is instantiated.

For instance, consider a simple do nothing library like the
following::

 #!r6rs
 (library (x)
 (export)
 (import (rnrs))
 (display "instantiated x!\n")
 )

If you now run the following script

::

 $ cat script.ss
 (import (for (x) expand run))

the message ``instantiated x!`` will be printed only *once* by Larceny, but
*twice* by PLT Scheme. For comparison, Ypsilon prints the message only
once (it has single instantiation semantics) and Ikarus does not print
any message at all (!), since the module is not used (it would print
the message only once if the module were used).

In other words, authors of portable libraries cannot rely on multiple
instantiation, nor on single instantiation.
|#
