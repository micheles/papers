About scheme implementations
=======================================================================

Scheme is a language with many implementations and with few
libraries. In this episode I will discuss the current situation and I
will give some useful indication to the Scheme beginner.

About Scheme implementations
-------------------------------------------------------------------

One of the biggest problems for the Scheme beginner is the choice
of the implementation. I did spend months on this issue and 
I have been on the verge of quitting many times. Since
implementations are fairly different and incompatible if you
make the "wrong" choice then you need to spend some effort to
reconvert your code. Nowadays in theory this is less of an issue,
since the R6RS_ report mandates an unique module system, but
many implentations are still not supporting it.
Moreover, in practice, in order
to perform enterprise programming tasks you will always be forced to rely on
implementation
specific libraries such as database drivers and frameworks.

.. _R6RS: http://www.r6rs.org/

I am sure you will ask me what is the right implementation.  The
answer is that there is no right implementation: it depends on your
needs. Every implementation has different advantages: 
there are implementations with a
very good interoperability with C, others well integrated in Java or
in .NET, others with a particularly good documentation, others with especially
useful libraries, but there is no single implementation with all the
features which is definitively superior to the others. You may use
more than an implementation at the time, but you need to careful in
the choice of the libraries you are going to use, if you are
interested in portability.

I cannot say I have tried all Scheme implementations (there are dozens
and dozens of them) so take my obvervations *cum grano salis*.  I did
try `PLT Scheme`_, Bigloo_, Chicken_, Guile_, Ikarus_ e Larceny_ which
are Open Source, multiplatform and free. Other major implementations
are `Chez Scheme`_ (the interpreter, called Petit Scheme is free, the
compiler is not) e and `MIT Scheme`_ (available with GPL licence)
but I have not tried them and I cannot say anything.  All the
implementations I tried (except Guile_) can be compiled and/or
generate C code, and are usually faster than Python. Bigloo_ in
particular is a "high performance compiler" optimized for floating
point computations. `PLT Scheme`_ provides an interpreter, a compiler
and an IDE called DrScheme: it is probably the biggest Scheme
implementation out there and it is also probably the most used
implementation and the one with most libraries.

`Chicken`_ is another big implementation: its major advantage
is its author Felix Wilkelmann who literally perform miracles to
support his users. I personally felt much more confortable in
the Chicken mailing list than in the PLT one, but it was a few
years ago and of course your mileage may vary. Anyway, Chicken
is the R5RS-compatible implementation
I like most since it has a very practical attitude: it
is written by people working in the industry and not in the academy.
Chicken is a compiler from Scheme to C and it is extremely easy to
write *wrappers* for C/C++ libraries. Moreover, there already hundreds
of interesting libraries available. They are called *eggs*, just as
in Python, and they work more a less in the same way. However, it
must be noticed that Chicken had eggs years before Python and more
rigths to use the name ;-)

Guile_ is the Scheme version sponsored by
the Free Software Foundation and it is used as scription language for
GIMP; it has been dreamed that Guile_ would become the main scripting
language for the FSF applications and that it would have replaced
Emacs Lisp in Emacs, but that never happened.

There are many other implementations I have not cited here, but my
advice is to stick to one of the major implementations, unless you
have some very special need. Notice, however, that in my experience, even
the so-called major implementations cannot compete with Python
for what concerns reliability and professionality. It is not just
that there are much less libraries of use for the enterprise
programmer (database, GUI, Web, etc), they are also more immature
and with more bugs. This clearly has to do with the fact that the
total number of users of Scheme (including all implementations)
is order of magnitudes smaller than the number of users of Python.
I must say however that the situation has improved a lot in recent years.

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/guile-title.jpg

.. _Chicken: http://www.call-with-current-continuation.org
.. _PLT scheme: http://www.plt-scheme.org
.. _Bigloo: http://www-sop.inria.fr/mimosa/fp/Bigloo/
.. _Guile: http://www.gnu.org/software/guile/guile.html
.. _Larceny: http://www.ccs.neu.edu/home/will/Larceny/
.. _Ikarus: http://www.cs.indiana.edu/~aghuloum/ikarus/
.. _Chez Scheme: http://www.scheme.com/
.. _MIT Scheme: http://www.gnu.org/software/mit-scheme/

About the library problem
---------------------------------------------------------------------

Libraries are the weak point of Scheme; there is simply no competition
between the number of available libraries for Python and for Scheme.
Let me consider just GUI libraries: in Python it is possible to
use practically any existing GUI toolkit. The most used are
Tk, GTK, Qt, WxPython, etc. If you are lucky, you can find
a Scheme implementation supporting one of those toolkits, but
certainly not all of them. There are Scheme implementations where
it is easy to write wrappers to C/C++ libraries, easier than in Python:
however, it is you who must write the wrapper, whereas in Python
there is always somebody who did the dirty work for you, and the
wrapper is kept up-to-date without any cost for you.

Clearly having a community split in at least a dozen of major
implementation does not help. You see the same issue, to a minor
degree, in Common Lisp too. Languages with a reference implementation
like Perl (which actually has a single implementation) or Python and
Ruby (with many implementations, but only one reference
implementation) have a substantial advantage for the point of view of
the enterprise programmer, since the community attention is focalized
on a single spot and everybody benefits from the work of everybody.

The Scheme community tried to improve the situation in various ways.
One problem is that the R5RS_ report is underspecified, so a mechanism
for proposing extensions to the standard was invented, under the name
of SRFI_ (Scheme Request For
Implementation). To a Pythonista SRFIs will look a lot
like PEPs_ (Python Enhancement Proposals). 
Everybody can submit a SRFI,
i.e. a paper describing a library or a set of improvements to the
language with the ambition of getting them into the standard. 
In principle the standard committee in charge of the next
*Revised Report* would pick up from the best SRFIs for inclusion
in the standard, but there is no obligation in this sense. 

As a matter of fact,
all existing implementations are making efforts to include the
most important SRFIs, so that code using the SRFI libraries has
better chances of being portable. Unfortunately, the R6RS editors
have ignored many existing SRFIs, reinventing them in imcompatible
ways and sometimes in inferior ways. The R6RS got a lots of critics
and some Scheme implementations claimed that they will never
be R6RS-compliant.

Every SRFI *must* be complemented by a working implementation, and
this is the reason from the *I* at the end. The implementation
must be as much portable as possible, therefore even if you are
using a Scheme implementation which does not support the SRFI you are
interested in natively, it is usually possible to port the SRFI
with little effort.
It is very important to study the most relevant SRFIs as soon
as you learn Scheme, since if you want to write any practical
application with it, you are going to need them.

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/chicken3b.png 

.. _R5RS: http://www.schemers.org/Documents/Standards/R5RS/HTML/
.. _SRFI: http://srfi.schemers.org/
.. _PEPs: http://www.python.org/dev/peps/

Additional difficulties
-----------------------------------------------------------------------

I did start playing with Scheme in 2003: at the time, I had
installation problems with all the implementations I tried (except
Guile_ which is typically pre-installed in Linux and Cygwin).
Nowadays things are simpler: basically all implementations provide
packages that can be installed on Linux systems via apt-get/yum or
other package managers, together with Windows and OS X installers.
One thing which is still giving problem is GNU readline_: whereas
usually the Python version you find pre-installed on your system
is readline-enabled, most Scheme
implementations do not enable readline by default for reason of
license, so you have
to download the readline-dev headers, edit the Makefile and
recompile everything by hand. This may be annoying, so I suggest
you to use rlwrap_ instead, which can be installed with
``apt-get`` in Debian/Ubuntu or with``fink`` on the Mac.

.. _readline: http://tiswww.case.edu/php/chet/readline/rltop.html
.. _rlwrap: http://utopia.knoware.nl/~hlub/rlwrap/man.html

rlwrap_ is a beatiful utility which can add readline support to each
command line program (such as an interactive Scheme interpreter) 
that does not support it. It is enough to type ``rlwrap <scheme-executable>``
and your REPL magically gains readline line editing, persistent
history and completion; moreover, you get *parens matching* for free,
which is invaluable in Scheme programming. I make heavy usage of all
these features.

By the way, I see now that I have used the term REPL
(Read-Eval-Print-Loop) which may be is unknown to a few readers; REPL
is just the Lisp name for what is called the interactive interpreter
or console in the Python world, the one with the ``>>>`` prompt. 
In Scheme the REPL is very
well integrated with Emacs, so that you can position the cursor right
after a closing parenthesis and send the corresponding expression to the
REPL with CTRL-x-e (in Python you are forced to select the expression
esplicitely instead, so that the user experience is not great as with Scheme).
Of course you need a good Scheme mode: the
default one is not so great and I use quack.el_ by Neil Van Dyke. The
Emacs support for Lispish languages is excellent (which is not
surprising at all, being Emacs written in a Lisp dialect) and I
definitely suggest you to use Emacs as your IDE for Scheme.
Of course, lots of people do not like Emacs, so you could use VI
instead, or even a specialized Scheme IDE such as DrScheme provided
by PLT Scheme. The important thing is to have support for parens matching.

Unfortunately, there is no equivalent to IPython and there will never
be, since the language does not have support for docstrings, nor the
introspection facilities of Python: you would need to switch to
Common Lisp with SLIME_ to find something comparable or even better.

.. _quack.el: http://www.neilvandyke.org/quack/
.. _SLIME: http://www.slime.org

Generally speaking (with some exception) the support you can get
for what concerns specific issues of a library is inferior
to the support you can get with Python. The
*comp.lang.scheme* newsgroup is friendly and can help
you a lot if you ask how to implement a given algorithm or
how a subtle Scheme construct works, but you should take in account
that the number of posters in comp.lang.scheme is perhaps the 5%
of the number of posters in comp.lang.python. On the other hand,
the Schemers are highly esperienced and competent people, so you
can get sound advice there.

All the Scheme implementations I tried are inferior to Python for what
concerns introspection and debugging capabilities. Tracebacks and
error messages are not very informative. Sometimes, you cannot even
get the number of the line where the error occurred; the reason is
that Scheme code can be macro-generated and the notion of line number
may become foggy. On the other hand, I must say than in the five
years I have being using Scheme (admittedly for toying and not for
large projects) I have seen steady improvement in this area.

To show you the difference between a Scheme traceback and a Python
traceback, here is an example with PLT Scheme, the most complete
Scheme implementation and perhaps the one with the best error
management::
 
 $rlwrap mzscheme 
 Welcome to MzScheme v4.1 [3m], Copyright (c) 2004-2008 PLT Scheme Inc.
 > (define (inv x) (/ 1 x))
 > (inv 0)
 /: division by zero

  === context ===
 /usr/local/collects/scheme/private/misc.ss:68:7

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/plt-green.jpg

As you see, there is no much information: in particular the
information about the name of the function where the error
occurred (``inv``) is lost and the line number/char number
refers to the ``read-eval-print-loop`` code. You may contrast
that with the Python traceback::

 $ python
 Python 2.5.1c1 (r251c1:54694M, Apr  5 2007, 12:45:14) 
 [GCC 4.0.1 (Apple Computer, Inc. build 5367)] on darwin
 Type "help", "copyright", "credits" or "license" for more information.
 >>> def inv(x): return 1/x
 ... 
 >>> inv(0)
 Traceback (most recent call last):
   File "<stdin>", line 1, in <module>
   File "<stdin>", line 1, in inv
 ZeroDivisionError: integer division or modulo by zero

I should mention however that PLT is meant to be run inside its own
IDE, DrScheme_. DrScheme highlights the line with the error and
include a debugger. However such functionalities are not that common
in the Scheme world and in my own experience it is much more difficult
to debug a Scheme program than a Python program.

The documentation system is also very limited as compared to Python:
there is no equivalent to pydoc, no help functionality from the REPL,
the concept of docstring is missing from the language. The road to
Scheme is long and uphill; from the point of view of the tools and
reliability of the implementations you will be probably better off
with Common Lisp. However, in my personal opinion, even Common Lisp is
by far less productive than Python for the typical usage of an
enterprise programmer. 

My interest here is different: I am not looking
for a `silver bullet`_, a language more productive than Python. My aim
is to find a language from which a Pythonista can learn something. And
certainly from Scheme we can learn *a lot*.  But you will see what in
the next episodes. See you soon!

.. _DrScheme: http://www.drscheme.org/
.. _silver bullet: http://en.wikipedia.org/wiki/Silver_bullet
