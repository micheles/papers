#|The R6RS module system
=========================================================
.. _Chicken: http://www.call-with-current-continuation.org/

For nearly 30 years Scheme lived without a standard module system.
The consequences of this omission were the proliferation of dozens
of incompatible module systems and neverending debates.
The situation changed with the R6RS report: nowadays Scheme *has*
an official module system, finally.

Unfortunately the official module system is *not* used
by all Scheme implementations, and it is quite possible that some implementation
will never support it. For instance `Chicken`_, a major implementation,
just released version 4, which includes a brand new module system
*not compatible* with the R6RS system.
You should be aware that the module system (and actually
the whole of the R6RS standard) is controversial, and there are good
reasons why it is so.

I cannot do anything about the political
issues, but I can do something about the technical issues, by
explaining the subtle points and by documenting the most common pitfalls.
It will take me six full episodes to
explain the module system and its trickiness, especially for macro
writers who want to write portable code.

.. image:: Jigsaw.png

Modules are not first class objects
-------------------------------------------------------------

Since the title of this series is *The Adventures of a Pythonista in
Schemeland* let me begin my excursion about the R6RS module
system by contrasting it with the Python module system.

The major difference between Python modules and Scheme modules is that
Python modules are first class runtime objects which can be passed and
returned from functions, as well as modified and introspected freely;
Scheme modules, instead, are compile time entities which cannot be
imported at runtime, nor passed to functions or returned from functions;
moreover they cannot be modified and cannot be introspected.

Python modules are so flexible because they are basically
dictionaries.  It would not be difficult to implement a Python-like
module system in Scheme, by making use of hash-tables, the equivalent
of dictionaries. However, the standard module system does not
follow this route, because Scheme modules may contain macros which are
not first class objects, therefore modules cannot be first class objects
themselves [some may argue that having macros which are not first
class objects is the root of all evil, and look for alternative routes
with macro-like constructs which are however first class objects; however,
I do not want to open this particular can of worms here].

Since Scheme modules are not first class objects it is impossible to add
names dynamically to a module, or to replace a binding with another, as
in Python. It is also impossible to get the list of names
exported by a module: the only way is to look at the export list
in the source code. It is also impossible to export all the names
from a module automatically: one has to list them all explicitly.

In general Scheme is not too strong at introspection, and that it is
really disturbing to me since it is an issue that could be easily
solved.  For instance, my ``sweet-macros`` library provides
introspection features, so that you can ask at runtime, for instance
from the REPL, what are the patterns and the literals accepted by a
macro, its source code and its associated transformer, even if the
macro is a purely compile time entity. It would be
perfectly possible to give an introspection API to every imported
module. For instance, every module could automagically define a
variable - defined both at runtime and compile time - containing the
full list of exported names and there could be some builtin syntax to
query the list.

But introspection has been completely neglected by the current
standard. One wonders how Schemers cope with large libraries/frameworks
like the ones we use every day in the enterprise world, which export
thounsands and thousands of names in hundreds and hundreds of modules.
Let's hope for something better in the future.

I also want to point out a thing that should be obvious:
if you have a Scheme library ``lib.sls`` which
defines a variable ``x``, and you import it with a prefix ``lib.``,
you can access the variable with the Python-like syntax
``lib.x``. However, ``lib.x`` in Scheme means something completely
different from ``lib.x`` in Python: ``lib.x`` in Scheme is just a name
with a prefix, whereas ``lib.x`` in Python means
"take the attribute ``x`` of the object ``lib``"
and that involves a function call.
In other words, Python must perform an hash table lookup everytime you
use the syntax ``lib.x``, whereas Scheme does not need to do so.

I should also points out that usually (and unfortunately) in
the Scheme world people do not use prefixes; by default all
exported names are imported, just as it is the case for Python
when the (discouraged) style ``from lib import *`` is used.

Compiling Scheme modules vs compiling Python modules
--------------------------------------------------------------

Let me continue my comparison between Python modules and Scheme
modules, by comparing the compilation/execution mechanism in the two
languages. I will begin from Python, by giving a simplified
description which is however not far for the truth.

When you run a script ``script.py`` depending on some library
``lib.py``, the Python interpreter searches fo a bytecode-compiled
file ``lib.pyc``, updated with respect to the the source file
``lib.py``; if it finds it, it imports it, otherwise it compiles the
source file *on-the-fly*, generates a ``lib.pyc`` file and imports it.
A bytecompiled file is updated with respect to the source file if it has been
generated *after* the source file; if you modify the source file, the
``lib.pyc`` file becomes outdated: the Python interpreter is smart
enough to recognize the issue and to seamlessly recompile ``lib.pyc``.

In Scheme the compilation process is very much *implementation-dependent*.
Here I will give some example of how things work in three representative
R6RS-conforming implementations, Ikarus, Ypsilon and PLT Scheme/mzscheme.

Ikarus has two modes of operation; by default it just compiles
everything from scratch, without using any intermediate file.
This is possible since the Ikarus compiler is very fast. However,
this mechanism does not scale; if you have very large libraries,
it does not make sense to recompile everything every time you add a
little script.
Therefore Ikarus (in the latest development version) added a mechanism
similar to the Python one; if you have a file ``script.ss`` which
depends on a library ``lib.sls`` and run the command 

::

 $ ikarus --compile-dependencies script.ss
 Serializing "./lib.sls.ikarus-fasl" ...

the compiler will automatically (re)generate a precompiled file
``lib.sls.ikarus-fasl`` from the source file ``lib.sls`` as needed, by
looking at the time stamps. Exactly the same as in Python.  The only
difference is that Python compiles to bytecode, whereas Ikarus compile
to native code.

Notice that whereas in theory Ikarus should
always be much faster of Python, in practice this is not guaranteed: a
lot of Python programs are actually calling underlying C libraries, so
that Python can be pretty fast in some cases (for instance in
numeric computations using numpy).

All I said for Ikarus, can be said from Ypsilon, with minor differences.
Ypsilon compiles to bytecode, like Python.
Precompiled files are automatically generated
without the need to specify any flag, as in Python; however they
are stored in a so called auto-compile-cache directory, which by
default is situated in ``$HOME/.ypsilon``. The location can
be changed by setting the environment variable ``YPSILON_ACC``
or by passing the ``--acc=dir`` argument to the Ypsilon interpreter.
It is possible to disable the cache and to clear the cache; if you
are curious about the details you should look at the Ypsilon manual
(``man ypsilon``).

PLT Scheme/mzscheme works in a slightly different way. The command

::

 $ plt-r6rs script.ss

interprets the script and its dependencies on the fly. The command


::

 $ plt-r6rs --compile script.ss


compiles the script and its dependencies, and stores the compiled file
in the *collects* directory, which on my system is in
``$HOME/.plt-scheme/4.1.2/collects``. Each library has its own
directory of compiled files.

Compiling is not the same than executing
-----------------------------------------------------------------

There are other similarities between a Python (bytecode)
compiler and a Scheme compiler.
For instance, they are both very permissive, in the sense that they flag
very few errors at compile time. Consider for instance the following
Python module::

 $ cat lib.py
 x = 1/0

The module contains an obvious error, that in principle should be
visible to the (bytecode) compiler. However, the compiler only checks
that the module contains syntactically correct Python code, it *does
not evaluate it*, and generates a ``lib.pyc`` file without
complaining::

 $ python -m py_compile lib.py # generates lib.pyc without errors

The error will be flagged at runtime, only when you import the module::

 $ python -c"import lib"
 Traceback (most recent call last):
   File "<string>", line 1, in <module>
   File "lib.py", line 1, in <module>
     x = 1/0
 ZeroDivisionError: integer division or modulo by zero 

R6RS Scheme uses a similar model. Consider for instance the library
 
::

 $ echo lib.sls
 #!r6rs
 (library (lib)
  (export x)
  (import (rnrs))
  (define x (/ 1 0))) 

and the script

:: 

 $ echo script.ss
 (import (rnrs) (lib))

You can compile the script and the library without seeing any error::

 $ plt-r6rs --compile script.ss
  [Compiling ./script.ss]
  [Compiling ./.plt-scheme/4.1.2/collects/lib/main.sls]

Running the script however raises an error::

 $  plt-r6rs  script.ss
 /: division by zero

Like in Python, the error is raised when the module is imported
(the technical name in Scheme is *instantiated*).

However, there is a gray area of the R6RS module system here, and
implementations are free to not import unused modules. To my knowledge,
Ikarus is the only implementation making using of this freedom.
If you run

::

 $ ikarus --r6rs-script script.ss

no error is raised. Ikarus is just *visiting* the
module, i.e. taking notes of the names exported
by it and of the dependencies, but the module is not
evaluated, because it is not used.  However, if you use it, for
instance if you try to access the ``x`` variable, you will get
the division error at runtime:

::
 
 $ echo script.ss
 (import (rnrs) (prefix (lib) lib:))
 (begin
   (display "running ...\n")
   (display lib:x))
 $  ikarus --r6rs-script script.ss
 Unhandled exception:
  Condition components:
    1. &assertion
    2. &who: /
    3. &message: "division by 0"
    4. &irritants: ()

Here I have used an import prefix ``lib:``, just to be more explicit.
Another difference between Ikarus and PLT is that in PLT both the
script and the library are compiled, whereas in Ikarus only the library
is compiled. In the next episodes we will see many other examples of differences
between R6RS-conforming implementations. 

----

*Acknowledgments*

All the *Adventures* have my name at the top and I take full
responsibility for the opinions and the mistakes. But for
the parts which are correct, I deserves little credit, since
most of the time I am just reporting advice which I have received
from the Scheme community, mostly from comp.lang.scheme and
ikarus-users, as well from private emails. This is true for all of
my *Adventures*, but especially
for the six episodes about the module system you are about to read.
I was very ignorant about the module system when I started this
project, and this work would not have been possible without the
help of Abdulaziz Ghuloum, Derick Eddington, Will Clinger, Eli
Barzilay, Matthew Flatt, André van Tolder and many others.
Thank you guys, you rock!
|#


; Derick Eddington:
; I like Ikarus's only-when-needed semantics because it seems better for
; supporting libraries which grow to have a ton of dependencies because
; they liberally use a thing from here and a thing from there (as they
; should be able to, IMO) and those dependencies have a lot of
; dependencies which aren't actually used by the things a particular
; program uses and so those transitive dependencies don't need to be
; instantiated because they're not actually used.  And I like it because I
; favor functional programming techniques.
