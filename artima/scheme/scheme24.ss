#|
Since phase separation and the transmission of side effects from
compile time to run-time is so tricky, I will spend another episode
on the subject, considering the problem of defining a
compile-time register of macros.

The registry example
-------------------------------------------------------

Suppose we define a registry module to register all the macro definitions
in our code as follows:

$$experimental/registry:

We can register a macro ``m1`` defined in the module ``def-m1`` as follows:

$$experimental/def-m1

Ther registry is a compile entity, but we would like to query it at
run-time, to introspect which macros have been defined during compilation.
This is clearly an impossible task, but we could try to perform it
in this way:

$$use-registry:

Notice also that I did specify importation at run time for the
``registry`` function, since it is called at runtime, i.e. not inside
macros. In Ikarus everything seems to work (in Ypsilon too)
and running the script returns something like

::

 registering #<syntax m1 [char 211 of .../experimental/def-m1.sls]>
 registering #<syntax m2 [char 256 of use-registry.ss]>
Registered 2 macro(s)

(notice the annotation about the position of the identifiers ``m1``
and ``m2`` in the source code of the script). However, you get the
correct result only by accident. Once you start compiling the
libraries

::

 $ ikarus --compile-dependencies use-registry.ss
 registering #<syntax m1 [char 211 of .../experimental/def-m1.sls]>
 registering #<syntax m2 [char 256 of use-registry.ss]>
 Serializing ".../experimental/def-m1.sls.ikarus-fasl" ...

you start getting a different result::

 $ ikarus --r6rs-script use-registry.ss
 registering #<syntax m2 [char 256 of use-registry.ss]>
 Registered 1 macro(s)

The problem is that the macro ``m1`` has been registered when the library
``def-m1`` has been compiled and we lost track of it in the second run
of the compiler. On the other hand, the macro ``m2`` is defined inside
the script and there

In PLT Scheme instead running the script returns a stable result::

 $ plt-r6rs use-registry.ss
 registering #<syntax:/home/micheles/.plt-scheme/4.0/collects/experimental/def-m1.sls:9:16>
 registering #<syntax:/home/micheles/gcode/artima/scheme/use-registry.ss:9:16>
 Registered 0 macro(s)

The result may look surprising at first, but it is actually correct.
The point is that there are two instances of the ``registry`` module
here. The first instance is the compile-time instance. The ``_registry``
variable of this instance is incremented by the register. However,
when we import the ``register`` function at run-time, we are actually
looking at the run-time instance of the ``registry`` module: in this
instance the ``_registry`` list has not been changed and has kept
its initial value, i.e. it is the empty list.

Incidentally, you may see here that the PLT Scheme representation of
syntax objects shows the line number and the column number of the
registered macros, as well as the module path.

Transferring information from compile time to runtime
-----------------------------------------------------------------

This example should have convinced you that there is no easy way to
transfer information from compile-time to run-time, if you want to
support separate compilation of modules. A tentative solution could
be to save compile-time information persistently in the file system.
For instance, out register could print the names of the registered
macros on a specific file at compilation-time. At runtime, those
names could be read from the file and introspected. However this
is brittle since it assumes the file to be still available at
run-time. This is not certain, since the file could have been removed
from the system, the directory structure could have changed between
compilation and execution or simply we could be running code which was
compiled in a different machine. Moreover this solution would not be
portable amongst different Scheme implementation, since the way
modules are instantiated is not standardized: in particular a module
could be instantiated many times in an implementation (that would
result in the macro names being written many times) and only once
in another implementation.

There is an extremely interesting post by Derick Eddington on
ikarus-users_ which shows this point very effectively: the number
of times a library is instantiated is very much implementation-dependent
and determining it is very far from trivial.

All considered, I would say that it is effectly impossible to transfer
information from compile-time to run-time reliably by using only
portable features of the current R6RS standard.

.. _ikarus-users: http://groups.google.com/group/ikarus-users/msg/7bfdb205a0935e91?hl=en
|#
