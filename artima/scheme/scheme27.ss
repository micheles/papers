#|Phase separation
===============================================================

Phase separation
-------------------------------------------------

Phase separation is one of the trickiest concepts in Scheme macrology,
and perhaps the one that gave me the most headaches when I was learning
macros. It still beats me sometimes. Actually, the concept it is not
that difficult, it is its practical implementation which is extremely
tricky because it is *unspecified by the Scheme standard* and
totally *implementation-dependendent*: worse than
that, the *same* implementation can implement phase separation *differently*
in compiled code and in interpreted code, and/or differently in the REPL 
and in scripts!


If you have a macro depending on helper functions, like
the previous one, you must put the helper functions in a separated
module if you want to ensure portability. Moreover, you must
import the helper functions with the syntax ``(for (module-name) expand)``
meaning that the helper functions are intended to be used at expand
time, in macros. Ikarus is quite forgiving and can just use a regular
import, but PLT Scheme and Larceny will raise an error if you do not
use the ``for expand``. A full description of the module system, with
all the gory details, will require six more episodes, and will constitute
part V of these *Adventures*.


Consider for instance this example in Ypsilon, which tries to implement
a macro registry: 

$$registry.ypsilon:

You can run the example and you will get

``registry: ((#<syntax m>)``

as result (notice however that if you comment out the macro use, i.e.
the ``(m)`` line, the registry will *not* be populated).
So everything seems to work as one would expect.
However, if you try to run the same
example in Ikarus or in PLT Scheme or in most other R6RS Scheme
implementations you will get an error. Let me
show the PLT error message message, which is rather
clear if you understand what phase separation is, wheread
the Ikarus error message is somewhat misleading for reasons misterious to me::

 $ plt-r6rs registry.ypsilon.ss 
 registry.ypsilon.ss:10:5: compile: unbound variable in module
 (in the transformer environment, which does not include the
 run-time definition) in: register

Ypsilon, as most interpreted Scheme implementations, has no phase separation:
there is no big difference between macros and functions, which are
simply recognized in the order given by their position in the source code.
In our example the ``register`` function comes before the ``m`` macro,
so it can be used in the right hand side of the macro definition.

An example will clarify the point. Suppose we define a registry module
as follows

$$experimental/registry:

(for convenience I am storing all this
code in a package called ``experimental``) and suppose we use it as follows:

$$use-registry.ikarus:

In Ikarus everything works fine (in Ypsilon too of course)
and running the script will return you something like

::

 registering #<syntax m [char 83 of use-registry.ikarus.ss]>
 (#<syntax m [char 83 of use-registry.ikarus.ss]>)

(notice the annotation about the position of the identifier ``m`` in
the source code of the script, a signature of the fact that ``m``
is a bona fide syntax object).

In PLT Scheme instead running the script raise an error::

 $ plt-r6rs use-registry.ikarus.ss
 use-registry.ikarus.ss:5:5: compile: unbound variable in module
 (transformer environment) in: register

.. image:: salvador-dali-clock.jpg

The problem is that PLT Scheme has *strong phase separation*: by default
names defined in external modules are imported *only* at runtime.
In some sense this is absurd since
names defined in an external pre-compiled modules
are of course known at compile time
(this is why Ikarus has no trouble to import them at compile time);
nevertheless PLT Scheme and Larceny Scheme forces you to specify
at which phase the functions must be imported. If you want to import
them at expansion time (the time when macros are processed; often
incorrectly used as synonymous for compilation time) you must say so:

$$use-registry.mzscheme:

Notice also that I did specify importation at run time for the
``registry`` function, since it is called at runtime, i.e. not inside
macros. If you run this script you will get::

 $ plt-r6rs use-registry.mzscheme.ss
 registering #<syntax:/home/micheles/gcode/artima/scheme/use-registry.mzscheme.ss:8:16>
 ()

The PLT Scheme representation of syntax objects shows the line number and
the column number, therefore you should interpret the previous out as
*I am registering the syntax object defined in the source file
use-registry.mzscheme.ss, at line 8 and column 16*, which corresponds
to the identifier ``m``.

This is close, but not quite cigar. The script now runs, but it
returns a rather unexpected empty list. The reason why the registry is
empty has nothing to do with phase separation, but rather
another "feature" of PLT Scheme (and only of PLT Scheme) which goes
under the name of multiple instantiation of modules. In practice,
importing a module in PLT Scheme imports *an independent
copy (instance) of the original module*.

The imported instance of the module includes a copy of all bindings
defined in the original module,
*including the internal bindings which are not exported*.
This remark explains why the registry is empty: the ``register``
functions changes the ``_registry`` list in the *current* instance,
but the value of ``_registry`` in the *original* instance (i.e. the
value returned by the ``(registry)`` function) is left unchanged and
is the same as at the beginning, i.e. the empty list.

However, I do not want to
complicate the explanation of phase separation now, which is already
complicated as it is, so let me defer a full explanation of this point
to a future episode of my *Adventures*.
|#

(import (rnrs) (sweet-macros) (aps list-utils) (aps easy-test) (aps compat)
        (for (aps lang) expand run))
