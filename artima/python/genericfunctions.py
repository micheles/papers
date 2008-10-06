"""\
In the last few weeks my collegues and me have been involved in a
project which required a command line interface. We did so by
leveraging on the ``cmd`` module in the standard Python library, to
which we added a network layer using Twisted. In the end, we had
classes interacting with the standard streams ``stdin``, ``stdout``,
``stderr`` and classes interacting with nonstandard streams such as
Twisted transports.  All the I/O was line oriented and we basically
needed three methods:

- ``print_out(self, text, *args)`` to print a line 
  on ``self.stdout``

- ``print_err(self, text, *args)`` to print a line 
  on ``self.stderr``

- ``readln_in(self)`` to read a line from ``self.stdin``

Depending on the type of ``self``, ``self.stdout`` was
``sys.stdout``, a Twisted transport, a log file or a file-like
wrapper to a database. Likewise for ``self.stderr`` and ``self.stdin``.

This is a problem that begs for generic functions. Unfortunately,
nobody in the Python world uses them (with the exception of P. J. Eby)
so for the moment we are using a suboptimal design involving mixins instead.
I am not really happy with that.
The aim of this blog post is to explain why a mixin solution is inferior
to a generic functions solution.

A mixin-oriented solution
---------------------------------------------------------

In the mixin solution, instead of generic functions one uses plain
old methods, stored into a mixin class. In this specific
case let me call the class ``StdIOMixin``:

$$StdIOMixin

where ``write`` is the following helper function:

$$write

``StdIOMixin`` is there to be mixed with other classes, providing
them with the ability to perform line-oriented I/O. By default, it
works on the standard streams, but if the client class overrides
the attributes ``stdout``, ``stderr``, ``stdin`` with suitable file-like
objects, it can be made to work with Twisted transports, files and 
databases. For instance, here is an example where ``stdout`` and ``stderr``
are overridden as files:

$$FileIO

>>> FileIO().print_out('hello!') # prints a line on out.txt

The design works and it looks elegant, but still I say that it is sub-optimal
compared to generic functions.

The basic problem of this design is that it adds methods
to the client classes and therefore it adds to the learning
curve. Suppose you have four client classes - one managing standard
stream, one managing files, one managing Twisted transports and one
managing database connections - then you have to add the mixin four
times. If you generate the documentation for your classes, the
methods ``print_out``, ``print_err`` and ``readln_in`` will be
documented four times. And this is not a shortcoming of pydoc:
the three methods are effectively cluttering your application
in a linear way, proportionally to the number of classes you have.

Moreover, those methods will add to the pollution of your class namespace,
with the potential risk on name collisions, especially in large frameworks.
In large frameworks (i.e. Plone, where a class my have 700+ attributes)
this is a serious problem: for instance, you cannot even use
auto-completion, since there are just too many completions. You must know
that I am `very sensitive to namespace pollution`_ so I always favor
approaches that can avoid it.

Also, suppose you only need the ``print_out`` functionality; the mixin
approach naturally would invite you to include the entire
``StdIOMixin``, importing in your class methods you don't need.  The
alternative would be to create three mixin classes ``StdinMixin``,
``StdoutMixin``, ``StderrMixin``, but most of the time you would need
all of them; it seems overkill to complicate so much your inheritance
hierarchy for a very simple functionality.

`As you may know`_, I am always looking for solutions avoiding 
(multiple) inheritance and 
generic functions fit the bill perfectly.

.. _As you may know: http://www.artima.com/weblogs/viewpost.jsp?thread=237121

A generic functions solution
-----------------------------------------------------------

I am sure most people do not
know about it, but Python 2.5 ships with an implementation of generic
functions in the standard library, in the ``pkgutil`` module (by P.J. Eby).
Currently, the implementation is only used
internally in ``pkgutil`` and it is completely undocumented;
therefore I never had the courage to use it in production, but
it works well. Even if it is simple, it is able to cover
most practical uses of generic functions. For instance, in our case we need
three generic functions:

::

 from pkgutil import simplegeneric

 @simplegeneric
 def print_out(self, text, *args):
     if args:
         text = text % args
     print >> self.stdout, text

 @simplegeneric
 def print_err(self, text, *args):
     if args:
         text = text % args
     print >> self.stderr, text

 @simplegeneric
 def readln_in(self):
     "Read a line from self.stdin (without trailing newline)"
     line = self.stdin.readline()
     if line:
         return line[:-1] # strip trailing newline

The power of generic functions is that you don't need to use inheritance:
``print_out`` will work on any object with a ``.stdout`` attribute
even if it does not derive from ``StdIOMixin``. For instance, if you
define the class 

$$FileOut

the following will print a message on the file ``out.txt``:

>>> print_out(FileOut(), 'writing on file') # prints a line on out.txt

Simple, isn't it?

Extending generic functions
---------------------------------------------------------

One advantage of methods with respect to ordinary functions is that they can
be overridden in subclasses; however, generic functions can be overridden
too - this is why they are also called multimethods. For instance,
you could define a class ``AddTimeStamp`` and override ``print_out``
to add a time stamp when applied to instances of ``AddTimeStamp``.
Here is how you would do it:

$$AddTimeStamp

::

 @print_out.register(AddTimeStamp) # add an implementation to print_out
 def impl(self, text, *args):
     "Implementation of print_out for AddTimeStamp instances"
     if args:
         text = text % args
     print >> self.stdout, datetime.datetime.now().isoformat(), text

and here in an example of use:

>>> print_out(AddTimeStamp(), 'writing on stdout')
2008-09-02T07:28:46.863932 writing on stdout

The syntax  ``@print_out.register(AddTimeStamp)`` is not the most beatiful
in the world, but its purposes should be clear: we are registering the
implementation of ``print_out`` to be used for instances of ``AddTimeStamp``.
When ``print_out`` is invoked on an instance of 
``AddTimeStamp`` a time stamp is printed; otherwise, the default implementation
is used. 

Notice that since the implementation of ``simplegeneric`` is simple,
the internal registry of implementations is not exposed and there is no 
introspection API; moreover, ``simplegeneric`` works for single dispatch
only and there is no explicit support for multimethod
cooperation (i.e. ``call-next-method``, for the ones familiar with
Common Lisp). Yet, you cannot pretend too much from thirty lines of code ;)

In this example I have named the ``AddTimeStamp`` implementation 
of ``print_out``
``impl``,  but you could have used any valid Python identifier, 
including ``print_out_AddTimeStamp``
or ``_``, if you felt so. Since the name ``print_out`` is explicit in
the decorator and since in practice you do not need to access the
explicit implementation directly, I have settled for a generic name like
``impl``. There is no standard convention since nobody uses
generic functions in Python (yet). 

There were plan to add generic functions to Python 3.0, but the
proposal have been shifted to Python 3.1, with a syntax yet to
define. Nevertheless, for people who don't want to wait,
``pkgutil.simplegeneric`` is already there and you can start
experimenting with generic functions right now.  Have fun!

.. _very sensitive to namespace pollution: http://stacktrace.it/articoli/2008/08/i-pericoli-della-programmazione-con-i-mixin3/
"""
import os, sys, datetime
    
def write(stream, text, *args):
    'Write on a stream by flushing if possible'
    if args: # when no args, do not consider '%' a special char
        text = text % args
    stream.write(text)
    flush = getattr(stream, 'flush', False)
    if flush:
        flush()

class StdIOMixin(object):
    "A mixin implementing line-oriented I/O"
    stdin = sys.stdin
    stdout = sys.stdout
    stderr = sys.stderr
    linesep = os.linesep
    
    def print_out(self, text, *args):
        "Write on self.stdout by flushing"
        write(self.stdout, str(text) + self.linesep, *args)

    def print_err(self, text, *args):
        "Write on self.stderr by flushing"
        write(self.stderr, str(text) + self.linesep, *args)

    def readln_in(self):
        "Read a line from self.stdin (without trailing newline) or None"
        line = self.stdin.readline()
        if line:
            return line[:-1] # strip trailing newline

class FileIO(StdIOMixin):
    def __init__(self):
        self.stdout = file('out.txt', 'w')
        self.stderr = file('err.txt', 'w')
    

from pkgutil import simplegeneric

@simplegeneric
def print_out(self, text, *args):
    if args:
        text = text % args
    print >> self.stdout, text

@simplegeneric
def print_err(self, text, *args):
    if args:
        text = text % args
    print >> self.stderr, text

@simplegeneric
def readln_in(self):
    "Read a line from self.stdin (without trailing newline) or None"
    line = self.stdin.readline()
    if line:
        return line[:-1] # strip trailing newline


class FileOut(object):
    def __init__(self):
        self.stdout = file('out.txt', 'w')
    
class AddTimeStamp(object):
    stdout = sys.stdout

@print_out.register(AddTimeStamp)
def print_out_AddTimeStamp(self, text, *args):
    if args:
        text = text % args
    print >> self.stdout, datetime.datetime.now().isoformat(), text
