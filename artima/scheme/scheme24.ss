#|Mutating variables across modules
===============================================================

There are situations where it is handy to mutate a global variable or
a data structure across modules, for instance to keep a registry of
objects, or a counter. However, direct mutation of exported variables
is forbidden by the R6RS standard. Consider for instance a module
exporting a variable ``x`` and a function ``incr-x`` with side
effects affecting that variable:

$$experimental/mod1:

This kind of side effect is ruled out by the R6RS
specification (section 7.2): *exported variables must be
immutable*. This is the reason why Ikarus, Ypsilon and Larceny reject
the code with errors like ``attempt to export mutated variable`` or
``attempt to modify immutable variable``.  The current SVN version PLT
Scheme also raises an error, but the official version
(4.1.5 at the time of this writing) is buggy (I submitted the bug report). 

Mutating internal variables
---------------------------------------------------

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

Everything works as one would expect.  However, things are trickier
when phase separation enters in the game.

Mutating variables across phases
--------------------------------------------------------

A Scheme implementation exhibits a *cross-phase side effect* if
mutating a variable at expand time affects the value of the same
variable at run-time. All R6RS implementations - except PLT Scheme
which uses different instances for different phases - may have
cross-phase side effects. On the other hand, in all R6RS
implementations cross-phase side effects can be avoided by using
separated compilation.

In order to give a concrete example, consider the following script::

$ cat use-mod2.ss

$$experimental/use-mod2:

Here we formally import the module ``mod2`` twice, both at run-time
and at expand time.  In PLT Scheme (which is the only implementation
with explicit phasing *and* multiple instantiation) there are two
fully separated instances of the module, and running the script
returns the following::

 $ plt-r6rs use-mod2.ss
 Instantiated mod2
 At expand-time x=1
 Instantiated mod2
 At run-time x=1

The fact that ``x`` was incremented at compile-time has no effect
at all at run-time, since the run-time variable ``x`` belongs to a completely
different instance of the module. In systems with single instantiation
instead, there is only a *single instance of the module for all phases*,
so that incrementing ``x`` at expand-time has effect at run-time::

 $ ikarus --r6rs-script use-mod2.ss
 Instantiated mod2
 At expand-time x=1
 At run-time x=2

You would get the same with Ypsilon and Larceny (Larceny has explicit
phasing but single instantiation and if you import a module in more
than one phase the variables are shared amongst the phases, so that
cross-phase side effects may happen).

The phase crossing only happens because the script is executed immediately
after compilation *in the same process*. Having compile-time
effects affecting run-time values is *evil*, since it breaks
separate compilation. If we turn the script into a library and we
compile it separately, it is clear than the run-time value of ``x``
cannot be affected by the compile-time value of ``x``
(maybe the code was compiled 10 years ago!).

Cross-phase side effects and separate compilation
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
different if we run the same code under different implementations.

For instance in Ypsilon the first time the script is run it prints three lines::

 $ ypsilon --r6rs use-mod3.ss
 Instantiated mod2
 At expand-time x=1
 At expand-time x=2
 At run-time x=3

However, if we run the script again it prints just two lines::

 $ ypsilon --r6rs use-mod3.ss
 Instantiated mod2
 At run-time x=1

The reason is that the first time Ypsilon compiles the libraries,
using the same module instance, so that there is a single ``x``
variable which is incremented twice at expand time and once at
run-time. The second time there is nothing to recompile, so only the
run-time ``x`` variable is incremented, and there is no reference to
the compile time instance.

The situation for Ikarus is slightly different. If we use the
``--r6rs-script`` flag we get the same output as before, when
``mod3`` was just a script::

 $ ikarus --r6rs-script use-mod3.ss
 Instantiated mod2
 At expand-time x=1
 At run-time x=211

However, this only happens because Ikarus is compiling all the libraries
at the same time (whole compilation). If we use separate compilation we get::

 $ ikarus --compile-dependencies use-mod3.ss
 Instantiated mod2
 At expand-time x=1
 Serializing "/home/micheles/gcode/scheme/experimental/mod3.sls.ikarus-fasl" ...
 Serializing "/home/micheles/gcode/scheme/experimental/mod2.sls.ikarus-fasl" ...

As you see, ``mod2`` the message ``At expand-time x=1`` is printed when
``mod2`` is compiled. If we run the script ``use-mod3.ss`` now, we
get just the run-time message::

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

I must notice that you could get the same behavior in non-PLT
implementations by spawning two separate processes, one after the
other: the first to compile the script and its libraries, and the
second to execute it. That would make sure that incrementing ``x`` in
the compilation phase would not influence the value of ``x`` at run-time.

Conclusion
---------------------------------------------------

This is the last episode of part IV. You should have an idea of how
the R6RS module system works, and you should be able to grasp the reasons
behind the differences between implementations.

In particular, it should
be clear that side effects are tricky, that you cannot rely on the
compilation/visiting/instantiation procedure being the same in
different implementations, that phase separation means different
things in different Scheme systems.

Still, I have left out many relevant things. I realized that
in order to say everything
there is to say about the subject, I should have at least doubled the
number of episodes.

I did not want to get lost in excessive detail.
Instead, I have decided to continue my series with another block of
episodes about macros, and to fill the remaining gaps about the
module systems in future Adventures.

So, as always, stay tuned and keep reading!
|#
