#|
The R6RS module system
=========================================================

Preamble
---------------------------------------------------------

For nearly 30 years Scheme lived without a standard module system.
The consequences of this omission were the proliferation of dozens
of different and incompatible module systems and neverending debates
about the best module system.
The situation changed with the R6RS report: nowadays Scheme *has*
am official module system, finally. 
Unfortunately the official module system is *not* used
by all the implementations, and it is possible that some implementation
will never support it; this is unfortunate, but there
is nothing we can do about it. 

In this fifth part of my *Adventures* I will discuss various technical
issues of the R6RS module system. 

It seems impossible, but after 30 years of discussion about the module
system, they still got it wrong (wrong according to my own personal
opinion of course)! It will takes me six full episodes to
explain the module system and its trickiness, especially for macro
writers who want to write portable code.

.. _5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

Compiling Scheme modules vs compiling Python modules
--------------------------------------------------------------

Since the title of this series is *The Adventures of a Pythonista in
Schemeland* I have decided to begin my escursion of the R6RS module
system by contrasting it with the Python module system.

How does Python work? All Pythonistas know the answer already, but let
me spell it out loud for the benefit of other readers, and allow me
to give a simplified description which however is not far 
for the truth.

When you run a script ``script.py`` depending on some library ``lib.py``,
the Python interpreter looks if there is a bytecode-compiled
file ``lib.pyc`` consistent with the source file ``lib.py``; if yes, 
it imports that, otherwise it compiles on-the-fly the source file, 
generates a ``lib.pyc`` file and imports it. 
A bytecompiled file is consistent with the source file if it has been
generated *after* the source file; if you modify the source file,
the ``lib.pyc`` file becomes outdated: the Python interpreter is
smart enough to recognize the issue and to seamlessly recompile ``lib.pyc``.

In Scheme the compilation process is very much *implementation-dependent*.
Here I will focus on the Ikarus mechanism, which is the most Pythonic you
can find. Ikarus has two modes of operation; by default it just compiles
everything from scratch, without using any intermediate file.
This is possible since the Ikarus compiler is very fast. However,
this mechanism does not scale; if you have very large libraries,
it does not make sense to recompile everything every time you write a
little script.
Therefore Ikarus (in the latest development version) added a mechanism
similar to the Python one; if you have a file ``script.ss`` which
depends on a library ``lib.sls`` and run the command 

::

 $ ikarus --compile-dependencies script.ss

the compiler will automatically (re)generate a precompiled file
``lib.sls.ikarus-fasl`` from the source file ``lib.sls`` as needed, by
looking at the time stamps. Exactly the same as in Python.  The only
difference is that Python compiles to bytecode, whereas Ikarus compile
to native code and therefore Ikarus programs are usually much faster
than Python programs. Notice that whereas in theory Ikarus should
always be much faster of Python, in practice this is not guaranteed: a
lot of Python programs are actually calling underlying C libraries, so
that Python can look pretty fast in some cases (for instance in
numeric computations using numpy).

Compiling is not the same than executing
-----------------------------------------------------------------

There other similarities between a Python compiler and a Scheme compiler.
For instance, they are both very permissive, in the sense that they flag
very few errors at compile time. Consider for instance the following
Python module::

 $ cat lib.py
 x = 1/0

The module contains an obvious error, that in principle should be
visible to the (bytecode) compiler. However, the compiler only checks
that the module contains syntactically correct Python code, it *does
not evaluate it*, and generates a ``lib.pyc`` file without
complaining:

 $ python -m py_compile lib.py # generates lib.pyc without errors

The error will be flagged at runtime, only when you import the module::

 $ python -c"import lib"
 Traceback (most recent call last):
   File "<string>", line 1, in <module>
   File "lib.py", line 1, in <module>
     x = 1/0
 ZeroDivisionError: integer division or modulo by zero 

Scheme uses a very similar model, but importing a module has a different
meaning. Consider for instance the library
 
::

 $ echo lib.sls
 (library (lib)
  (export x)
  (import (rnrs))
  (define x (/ 1 0))) 

which compiles correctly and the script

:: 

 $ echo script.ss
 (import (rnrs) (lib))

You can compile the library and run the script without seeing any error::

 $ ikarus --compile-dependencies script.ss
 Serializing "./lib.sls.ikarus-fasl" ... 
 $ ikarus --r6rs-script script.ss

The difference with Python is the following: in Python, importing a module
means *evaluating* it; in Scheme importing a module means *visiting* it,
i.e. taking notes of the names exported by the module and of all its
dependencies; however, the module is not evaluated, unless it is used.
In particular, only when you try to access the ``x`` variable, you will
get the division error at runtime:

::
 
 $ echo script.ss
 (import (rnrs) (prefix (lib) lib:))
 (display "running ...\n")
 (display lib:x)
 $  ikarus --r6rs-script script.ss
 Unhandled exception:
  Condition components:
    1. &assertion
    2. &who: /
    3. &message: "division by 0"
    4. &irritants: ()

Notice that I have imported the names in ``lib`` with a prefix,
to stay closer to the Python style, but usually (and unfortunately) in
the Scheme world people do not use prefixes; by default all
exported names are imported, just as it is the case for Python
when the (discouraged) style ``from lib import *`` is used.

Is it a good thing to have so little checking at compile-time?
------------------------------------------------------------------------

I asked myself why Scheme compilers (but also the Python compiler)
are so stupid that they cannot recognize obvious errors like the
zero division error just discussed. I could not find an answer
so I asked on the Ikarus mailing list. It turns out the compilers
are not stupid at all: they can recognize the zero division error,
but they cannot signal it since it is forbidden by the Scheme
specifications. For instance, Leppie, the implementor of
IronScheme wrote:

 In IronScheme, if I can detect there is an issue at compile
 time, I simply defer the computation to the runtime, or could even
 just convert it into a closure that will return an error. This is only
 one of the things that make Scheme quite hard to implement on a statically
 typed runtime such as the CLR, as it forces me to box values at method
 boundries and plenty type checking at runtime.

whereas Abdul Aziz Ghoulum wrote:

 Actually, Ikarus does some type checking, and it does
 detect the division by 0.  It however cannot do anything
 about it in this case since Scheme requires that the
 exception be raised when the division operation is
 performed at run time.

Aziz went further and explained to me the rationale for
the current specification. The reason is that we want
a function like

 ``(define (raise-zero-division-error) (/ 1 0))``

to be compilable. Of course, ``raise-zero-division-error`` will raise
an error when called, but the function itself is a valid compilable procedure.
You can think of a module like a giant thunk; using a module calls the
thunk and possibly raises errors at runtime, but the module per se is
compilable even if contains errors which are detectable at compile time.

This evaluation strategy keeps the compiler simple: we know that the
compiler will just expand the macros, but will not perform any evaluation.
This semantics also enable *cross compilation*: the compile time structure
will be compiled independently from the architecture, whereas the
runtime structures will be compiled differently depending on the
architecture of the target processor.

.. _cross compilation: http://chicken.wiki.br/cross-compilation

Modules are not first class objects
-------------------------------------------------------------

There is a major difference between Python modules and Scheme
module: Python modules are first class runtime objects which can be passed
and returned from functions, as well as modified and introspected freely;
Scheme modules instead are
compile time entities which are not first class objects, cannot
be modified and cannot
be introspected.

Python modules are so flexible because they are basically dictionaries
and it would not be difficult to implement a Python-like
module system in Scheme, by making use of hash-tables, the equivalent
of Python dictionaries. However, the standard module system does
not follow this route, for two reasons:

1. Performance, Python must perform an hash table lookup everytime you
   use the syntax ``lib.x``, whereas Scheme does not need to do so.
2. Scheme modules may contain macros which are not first class objects,
   therefore they cannot be first class objects themselves.

It should be clear that ``lib:x`` in Scheme means something completely
different from ``lib.x`` in Python: ``lib:x`` is just a name with a
prefix, whereas ``lib.x`` means "take the attribute ``x`` of the object
``lib``" and that involves a function call. It is possible to add
names dinamically to a Python module; it is impossible to do so
for a Scheme module. It is also impossible to get the list of names
exported by a module: the only way is to look at the export list
in the source code. That sucks, I know.

In general Scheme is not highly regarded for its introspection
features, and that it is really disturbing since it is an issue that
could be easily solved.  For instance, my ``sweet-macros`` library
provides introspection features, so that you can ask at runtime, for
instance from the REPL, what are the patterns and the literals
accepted by a macro, and even its source code, even if the macro is a
purely compile time entity. Therefore, it would be perfectly possible
to give an introspection API to every imported module. For instance,
every module lib could automagically define a macro ``introspect-lib``
to be used for introspection, with features like returning the full
list of exported names, checking if a name is in the list, and so
on. But introspection has been completely neglected by the current
standard. One wonders how Schemers cope with large libraries/frameworks
like the ones we use every day in the enterprise world, which export
thounsands and thousands of names in hundreds and hundreds of modules.
Let's hope for something better in the future.
|#
