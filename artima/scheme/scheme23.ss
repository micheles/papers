#|
Side effects in modules
===============================================================

In functional programs there are no side effects; in real life programs
however there are side effects, and we must be able to cope with them.
The way the module system copes with side effects is quite tricky and
implementation-dependent. The goal of this episode is to shed some
light on the subject.

Side effects and interpreter semantics
------------------------------------------------------

Let me start with a simple example. Consider a module exporting a variable
(``x``) and a function with side effects affecting that variable (``incr-x``)):

$$experimental/mod1:

This kind of side effect is ruled out by the R6RS specification (section 7.2):
*exported variables must be immutable*. This is the reason why Ikarus, Ypsilon
and Larceny reject the code with errors like 
``attempt to export mutated variable`` or 
``attempt to modify immutable variable``.
On the other hand PLT Scheme compiles the code without raising any
warning, therefore even this simple case is tricky and exposes
a bug in the PLT implementation of the module system :-(

Consider now a module exporting a function with side effects affecting a
non-exported (i.e. private) variable:

$$experimental/mod2:

This is a valid library which compiles correctly. The accessor ``get-x``
gives access to the internal variable ``x``. We may import it
at the REPL and experiment with it:

.. code-block:: scheme

 > (import (experimental mod2))
 > (get-x)
 0
 > (incr-x)
 1
 > (incr-x)
 2
 > (get-x)
 2

Everything works as you would expect.

As always, things are trickier in scripts, when compiler semantics and
phase separation enters in the game.

Side effects and compiler semantics
------------------------------------------------------

Consider the following script:

$$experimental/use-mod2:

Here we import the module ``mod2`` twice, both at run-time and at expand time.
In Scheme implementations with multiple instantiation there are two fully
separated instances of the module, and running the script returns
what you would expect::

 $ plt-r6rs use-mod2.ss 
 At expand-time x=1
 At run-time x=1

The fact that ``x`` was incremented at compile-time has no effect
at all at run-time, since the run-time variable ``x`` belongs to a completely
different instance of the module. In system with single instantiation
instead, there is only a *single instance of the module for all phases*,
so that incrementing ``x`` at expand-time has effect at runtime::

 $ ikarus --r6rs-script use-mod2.ss
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
(maybe the code was compiled 10 years ago!) and it
must use a separate instance of the imported module.

Side effects and separate compilation
-------------------------------------------------------------

Let me explain in detail how separate compilation works in Ikarus,
Ypsilon and PLT Scheme. Suppose we turn the previous script into a library

$$experimental/use-mod3:

and let us invoke this library though a script ``use-mod3.ss``:

$$experimental/use-mod3:

If we use PLT Scheme, nothings changes::

 $ plt-r6rs use-mod3.ss
 At expand-time x=1
 At run-time x=1

This is expected: turning a script into a library did not make
anything magic happens. On the other hand, things are very
different if we run the same code under Ypsilon.
The first time the script is run it prints three lines::

 $ ypsilon --r6rs use-mod3.ss 
 At expand-time x=1
 At expand-time x=2
 At run-time x=3

However, if we run the script again it prints just one line::

 $ ypsilon --r6rs use-mod3.ss 
 At run-time x=1

The reason is that the first time Ypsilon compiles the libraries, using
the same module instance, so that there is a single ``x`` variable which
is incremented twice at expand time - the first time when ``mod2``
is imported and the second time when ``mod3`` is imported - and
once at run-time. The second time there is nothing
to recompile, so only the runtime ``x`` variable is incremented, and
there is no reference to the compile time instance.

The situation for Ikarus is subtler. Apparently we get the same
as before, when ``mod3`` was just a script::

 $ ikarus --r6rs-script use-mod3.ss 
 At expand-time x=1
 At run-time x=2

However, this only happens because Ikarus is compiling all the libraries
at the same time. If we use separate compilation we get::

 $ ikarus --compile-dependencies use-mod3.ss 
 At expand-time x=1
 Serializing "/home/micheles/gcode/scheme/experimental/mod3.sls.ikarus-fasl" ...
 Serializing "/home/micheles/gcode/scheme/experimental/mod2.sls.ikarus-fasl" ...

As you see, ``mod2`` the message ``At expand-time x=1`` is printed when
``mod2`` is compiled. If we run the script ``use-mod3.ss`` now, we
get just the runtime message::

 $ ikarus --r6rs-script use-mod3.ss 
 At run-time x=1

In Ikarus, Ypsilon and Larceny, the same invocation of this script
returns different results, depending if the libraries have been
precompiled or not. This is ugly and error prone. The multiple
instantiation mechanism of PLT Scheme has been designed to avoid this
problem: in PLT one consistently gets always the same
result.

On could get the same in non-PLT implementations by spawning
two separate processes, run one after the other: the
first to compile the script and its libraries, and the second to
execute it. That would make sure that incrementing
``x`` in the expansion phase would not influence the value of ``x``
at runtime.

My wishlist
---------------------------------------------------

I have discussed a large spectrum of solutions to the module system
problem, but I did not find any that satisfies me completely.
I have a personal wishlist of features.

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
  that we need a tower of metalevels. Two phases are more than enough.
  We can just perform the compilation in a different process
  than the evaluation process. In this way the
  variables can be imported in all phases, with
  implicit phasing semantics, but modifying a variable at expand time
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
