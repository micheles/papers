#|Separate compilation
==================================

.. _19: http://www.artima.com/weblogs/viewpost.jsp?thread=251476
.. _You want it when?: http://www.cs.utah.edu/plt/publications/macromod.pdf
.. _SRFI-19: http://srfi.schemers.org/srfi-19/srfi-19.html

I have discussed many times the concept of *time* in Scheme: there
is a run-time, an expand-time, and a whole set of times
associated to the metalevels. Moreover, when you take in consideration
use separate compilation there is yet another set of times: the times when your
libraries are separately compiled.

.. image:: time.jpg

Things are even trickier if the separately compiled libraries
are defining macros used in client code, since yet another concept of
time enters in the game, the concept of *visit time.

Suppose we have a low level library ``L``,
compiled yesterday, defining a macro we want to use in another
middle level library ``M``, to be compiled today. The middle level
library needs to know about the macro definition, because it has
to expand code using it during its compilation. Therefore, the compiler
must look at the low level library and re-evaluate the macro
definition today (this process is called *visiting*). The
visit time is different from the time of the
compilation of ``L`` and it happens just before the compilation of ``M``.
The example below should make things clear.

Consider a simple low level library ``L``, definining a macro ``m``
and an integer variable ``a``:

$$experimental/L:

You may compile it with PLT Scheme::

 $ plt-r6rs --compile L.sls 
  [Compiling /usr/home/micheles/gcode/scheme/experimental/L.sls]
 visiting L

Since the right hand side of a macro definition is evaluated at
compile time the message ``visiting L`` is printed during compilation,
as expected.

Here is a simple middle level library using the macro ``m``:

$$experimental/M:

I have used the ``(when #f (m))`` trick to make absolutely clear that
the macro is expanded even if it is in code which will never be
used at runtime. Still, the compiler needs to visit ``L`` in order
to compile ``M``. This is actually what happens::

 $ plt-r6rs --compile M.sls 
  [Compiling /usr/home/micheles/gcode/scheme/experimental/M.sls]
 visiting L

If you comment the line with the macro call the compiler in principle
does not need to visit ``L`` anymore; some implementations may take
advantage of this fact (Ypsilon and Ikarus do). However, PLT Scheme will
continue to visit ``L`` in any case.

Import semantics and portability gotchas
-----------------------------------------------

It is time to ask ourselves the crucial question:
what does it mean to *import* a library?

For a Pythonista, things are simple: importing a library means
executing it at runtime.  For a Schemer, things are complicated:
importing a library implies that some operation are performed at
compile time - such as looking at the exported identifiers and at the
dependencies of the library - but there is also a lot of unspecified
behaviour which may happen both a compile-time - a library may be
visited, i.e. its macro definitions can be re-evaluated - and at run-time
- a library may be instantiated. Different things happens in different
implementations.

The example of the previous paragraph if very useful if you want to
have an idea of what it portable behaviour and what is not.

Let me first consider what happens in Ikarus.

If I want to compile ``L`` and ``M`` in Ikarus, I need to introduce
a helper script ``H.ss``, since Ikarus has no direct way to compile
a library from the command line:

$$experimental/H:

Here is what we get::

 $ ikarus --compile-dependencies H.ss
 visiting L
 Serializing "/home/micheles/gcode/scheme/experimental/M.sls.ikarus-fasl" ...
 Serializing "/home/micheles/gcode/scheme/experimental/L.sls.ikarus-fasl" ...

Ikarus is lazier than PLT and you can check that if you comment the
line invoking the macro in ``M.sls`` and you recompile the dependencies,
then the library ``M`` is not visited. The same for Ypsilon. You may
also check that if you introduce a dummy macro in ``M``, depending on
the variable ``a`` defined in ``L`` (for instance if you add a line
``(def-syntax dummy (lambda (x) a))``) then the library ``L``
needs to be instantiated just to compile ``M``.

Ypsilon does not have a switch to compile a library without
executing it - even if this is possible by invoking the low level
compiler API - so we must execute ``H.ss`` to compile its dependencies::

 $ ypsilon --r6rs H.ss
 L instantiated
 visiting L
 M instantiated
 42

There are several things to notice here, since the output of Ypsilon is
quite different from the output of Ikarus

::

 $ ikarus --r6rs-script H.ss
 L instantiated
 42

and the output of PLT::

 $ plt-r6rs H.ss
 visiting L
 visiting L
 L instantiated
 M instantiated
 42

The first thing to notice is that in Ikarus and in PLT we relied on the
fact that the libraries were precompiled, so in order to perform a fair
comparison we must run Ypsilon again (this second time the libraries
L and M will be precompiled)::

 $ ypsilon --r6rs H.ss
 L instantiated
 M instantiated
 42

You my notice that this time the library ``L`` is not visited: it was visited
the first time, in order to compile ``M``, but there is no need
to do so now. During compilation of ``M`` macros has been expanded and
the bytecode of ``M`` contains the expanded version of the library; moreover
the helper script ``H`` does not use any macro so it does not really need
to visit ``L`` or ``M`` to be compiled. The same happens for Ikarus.
PLT instead visits ``L`` twice
to compile ``H.ss``. In PLT all dependencies (both direct and indirect)
are always visited when compiling. If we compile
the script once and for all

::

 $ plt-r6rs --compile H.ss
  [Compiling /usr/home/micheles/gcode/scheme/experimental/H.ss]
  [Compiling /home/micheles/.plt-scheme/4.1.5.5/collects/experimental/M.sls]
 visiting L
 visiting L

obviously the ``visiting L`` message will not be printed::

 $ plt-r6rs H.ss
 L instantiated
 M instantiated
 42

Having performed the right number of compilations now
the output of PLT and Ypsilon are the same; nevertheless, the output of
Ikarus is different, since Ikarus does not instantiate the middle level
library ``M``. The reason is the implicit phasing semantics of Ikarus
(but other implementations based on psyntax would behave the same): the
helper script ``H.ss`` is printing the variable ``a`` which really
comes from the library ``L``. Ikarus is clever enough to recognize this
fact and lazy enough to avoid instantiating the ``M`` library without
need.

This is both good and bad: it is good if ``M`` is
really unneeded; it is bad if ``M`` has some side effect,
since the side effect will misteriously disappear: in this example
the side effect is just printing the message ``M instantiated``, in
more sophisticated examples the side effect could be writing a log
on a database, or initializing some variable, or registering an object,
or something else. For instance,
suppose you want to collect a bunch of functions into
a global registry acting as a dictionary of functions.
You may do so as follows:

.. code-block:: scheme

 (library (my-library)
 (export)
 (import (registry))

 (define (f1 ...)  ...)
 (registry-set! 'f1 f1)

 (define (f2 ...) ...)
 (registry-set! 'f2 f2)

 ...
 )

The library here does not export anything, since it relies on side effects
to populate the global registry of functions; the idea is to access the
functions later, with a call of kind ``(registry-ref <func-name>)``.
This design as it is is not portable to systems based on psyntax, because
such systems will not instantiate the library (the library does not export
any variable, nothing of the library can be used in client code!).
This can easily be fixed, by introducing an initialization function
to be exported and called explicitly from client code, which is a
good idea in any case.

Analogously, a library based on side effects at visit time,
i.e. in the right hand side of macro definitions, is not portable,
since systems based on psyntax will not visit a library with
macros which are not used. This is relevant if you want to use the
technique described in the `You want it when?`_ paper: in order
to make sure that the technique work on systems based on psyntax, you 
must make sure that the library exports at least one macro which
is used in client code. Curious readers will find the gory
details `in this thread`_ on the PLT mailing list.

.. _in this thread: http://groups.google.com/group/plt-scheme/browse_frm/thread/c124fa9c48dc5b6a?hl=en#

Generally speaking, you cannot
rely on the number of times a library will be instantiated,
even within the *same* implementation!
Abdulaziz Ghuloum gave a nice example in the Ikarus and PLT lists. You
have the following libraries:

.. code-block:: scheme

 (library (T0) (export) (import (rnrs)) (display "T0\n"))
 (library (T1) (export) (import (for (T0) run expand)))
 (library (T2) (export) (import (for (T1) run expand)))
 (library (T3) (export) (import (for (T2) run expand)))

and the following script:

.. code-block:: scheme

 #!r6rs
 (import (T3))

Running the script (without precompilation) results in printing T0::

 0 times for Ikarus and Mosh
 1 time for Larceny and Ypsilon
 10 times for plt-r6rs
 13 times for mzscheme
 22 times for DrScheme

T0 is not printed in psyntax-based implementations, since it does not export
any identifier that can be used. T0 is printed once in Larceny and Ypsilon
since they are single instantiation implementations with eager import.
The situation in PLT Scheme is subtle, and you can find a detailed explaination
of what it is happening `in this other thread`_. Otherwise, you will have to
wait for the next (and last!) episode of this series, where I will explain
the reason why PLT is instantiating (and visiting) modules so much.

.. _in this other thread: http://groups.google.com/group/ikarus-users/browse_frm/thread/b07ef7266988bd1a?hl=en#
|#

