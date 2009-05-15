#|
Cross-phase side effects
===============================================================

.. _previous episode:

In the `previous episode`_ we saw that PLT Scheme performs much more
instantiation and visiting of modules than other implementations. The
reason for such behavior is that the PLT people want to avoid cross-phase
side effects which can be quite surprising, especially in presence of
separate compilation. 
For sake of simplicity I will focus only on side effects related to
the mutation of a variable, i.e. ``set!``-style side effects.

Experimenting with side effects
------------------------------------------------------

Let me start first with a simple example of side effects which are excluded by
the R6RS standard. Consider a module exporting a variable
(``x``) and a function with side effects affecting that variable (``incr-x``)):

$$experimental/mod1:

This kind of side effect is ruled out by the R6RS specification (section 7.2):
*exported variables must be immutable*. This is the reason why Ikarus, Ypsilon
and Larceny reject the code with errors like 
``attempt to export mutated variable`` or 
``attempt to modify immutable variable``.
The current SVN version PLT Scheme also raises an error, but the official version
(4.1.5 at the time of this writing) is still buggy (I submitted the bug report
and it was immediately fixed). 

Consider now a module exporting a function with side effects affecting a
non-exported (i.e. private) variable:

$$experimental/mod2:

This is a valid library which compiles correctly. The accessor function ``get-x``
gives access to the internal variable ``x``. We may import it
at the REPL and we may experiment with it:

.. code-block:: scheme

 $ ikarus
 > (import (experimental mod2)); this does not instantiate mod2 immediately
 > (get-x); now mod2 must be instantiated
 Instantiated mod2
 0
 > (incr-x)
 1
 > (incr-x)
 2
 > (get-x)
 2

Everything works as one would expect.

As always, things are trickier when compiler semantics and
phase separation enters in the game.

Side effects and the compile-time/run-time confusion
--------------------------------------------------------

Consider the following script::

$ cat use-mod2.ss

$$experimental/use-mod2:

Here we formally import the module ``mod2`` twice, both at run-time
and at expand time.  In Scheme implementations with multiple
instantiation (in practice, the only case is PLT Scheme) there are two
fully separated instances of the module, and running the script
returns what you would expect::

 $ plt-r6rs use-mod2.ss
 Instantiated mod2
 At expand-time x=1
 Instantiated mod2
 At run-time x=1

The fact that ``x`` was incremented at compile-time has no effect
at all at run-time, since the run-time variable ``x`` belongs to a completely
different instance of the module. In system with single instantiation
instead, there is only a *single instance of the module for all phases*,
so that incrementing ``x`` at expand-time has effect at runtime (I call
this behavior a *cross-phase* side effect)::

 $ ikarus --r6rs-script use-mod2.ss
 Instantiated mod2
 At expand-time x=1
 At run-time x=2

You would get the same with Ypsilon and Larceny (Larceny has explicit
phasing but single instantiation and if you import a module in more
than one phase the variables are shared amongsts the phases).

This only works because the script is executed immediately
after compilation *in the same process*. However, having compile-time
effects affecting run-time values is *wrong*, since it breaks
separate compilation. If we turn the script into a library and we
compile it separately, it is clear than the run-time value of ``x``
cannot be affected by the compile-time value of ``x``
(maybe the code was compiled 10 years ago!).

Side effects and separate compilation
-------------------------------------------------------------

Let me explain in detail how separate compilation works in Ikarus,
Ypsilon and PLT Scheme. Suppose we turn the previous script into a library::

 $ cat mod3.ss

$$experimental/mod3:

and let us invoke this library though a script ``use-mod3.ss``:

$$experimental/use-mod3:

If we use PLT Scheme, the value of ``x`` is the same::

 $ plt-r6rs use-mod3.ss
 Instantiated mod2
 At expand-time x=1
 Instantiated mod2
 Instantiated mod2
 At run-time x=1

This is expected: turning a script into a library did not make
anything magic happens (actually mod2 is being instantiated once more
during the compilation of mod3, but that should not be surprising).
On the other hand, things are very
different if we run the same code under Ypsilon or Ikarus.
The first time the script is run it prints three lines::

 $ ypsilon --r6rs use-mod3.ss
 Instantiated mod2
 At expand-time x=1
 At expand-time x=2
 At run-time x=3

However, if we run the script again it prints just two lines::

 $ ypsilon --r6rs use-mod3.ss
 Instantiated mod2
 At run-time x=1

The reason is that the first time Ypsilon compiles the libraries, using
the same module instance, so that there is a single ``x`` variable which
is incremented twice at expand time - the first time when ``mod2``
is imported and the second time when ``mod3`` is imported - and
once at run-time. The second time there is nothing
to recompile, so only the runtime ``x`` variable is incremented, and
there is no reference to the compile time instance.

The situation for Ikarus is slightly different. If we use the
``--r6rs-script`` flage we get the same output as before, when
``mod3`` was just a script::

 $ ikarus --r6rs-script use-mod3.ss
 Instantiated mod2
 At expand-time x=1
 At run-time x=2

However, this only happens because Ikarus is compiling all the libraries
at the same time (whole compilation). If we use separate compilation we get::

 $ ikarus --compile-dependencies use-mod3.ss
 Instantiated mod2
 At expand-time x=1
 Serializing "/home/micheles/gcode/scheme/experimental/mod3.sls.ikarus-fasl" ...
 Serializing "/home/micheles/gcode/scheme/experimental/mod2.sls.ikarus-fasl" ...

As you see, ``mod2`` the message ``At expand-time x=1`` is printed when
``mod2`` is compiled. If we run the script ``use-mod3.ss`` now, we
get just the runtime message::

 $ ikarus --r6rs-script use-mod3.ss
 Instantiated mod2
 At run-time x=1

In Ikarus, Ypsilon and Larceny, the same invocation of this script
returns different results for the variable ``x``, depending if the
libraries have been precompiled or not. This is ugly and error
prone. The multiple instantiation mechanism of PLT Scheme has been
designed to avoid this problem: in PLT one consistently gets always
the same result, which is the result you would get with separation
compilation.

I must notice that one could get the same behavior in non-PLT
implementations by spawning two separate processes, run one after the
other: the first to compile the script and its libraries, and the
second to execute it. That would make sure that incrementing ``x`` in
the expansion phase would not influence the value of ``x`` at runtime.

My wishlist
---------------------------------------------------

I have discussed a large spectrum of different implementations of the
R6RS module system, but I must notice that did not find any that
satisfies me completely.  I have a personal wishlist of features.

- interpreter semantics and the REPL

  I think that the behavior of REPL should
  not be too different from the behavior of scripts; in particular,
  I like that in PLT functions defined in the REPL
  are not available in macros. This may be inconvenient, but
  I think it is pedagogically
  useful, since it forces beginners to think about phase separation
  early on and to have a consistent model of what will happen once
  they compile the script. I started programming in Scheme
  with implementations where the REPL behavior was different
  from the compiled behavior and it was very difficult for me
  to understand why my "correct" scripts did not compile.

- phase separation and multiple instantiation

  I agree with the PLT people that expand-time variables and run-time
  variables should live in different instances, however I disagree
  that we need a tower of meta-levels. Two phases are more than enough.
  We can just perform the compilation in a different process
  than the evaluation process. In this way the variables can be
  imported in all phases, but modifying a variable at expand time
  cannot influence its runtime counterpart.

- missing features from the standard

  I think the standard lacks many important features. I have
  already noticed that I would welcome the ability to export all names from a
  library ``(export *)`` and the ability to introspect the names exported
  by library. In addition,
  I would welcome a mechanism to write helper functions for macros *in the
  same file* the macros are defined, if one wants to. One way to implement this
  feature is to standardize the ability of writing multiple libraries in
  the same file, which is already provided by some implementations.
  Another way is to introduce at ``define-at-all-phases`` form.
|#
