#|The evaluation strategy of Scheme programs
================================================

The Scheme module system is complex, because of the
complications caused by macros and because of the want of
separate compilation and cross compilation.
However, fortunately, the complication
is hidden, and the module system works well enough for many
simple cases. The proof is that we introduced the R6RS module
system in episode 5_, and for 20 episode we could go on safely
by just using the basic import/export syntax. However, once
nontrivial macros enters in the game, things start to become
interesting.

.. _5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699
.. _R6RS document: http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html#node_idx_1142

Interpreter semantics vs compiler semantics
------------------------------------------------------------------

One of the trickiest things about Scheme, coming from Python, is its
distinction between *interpreter semantics* and *compiler semantics*.

To understand the issue, let me first point out that having
interpreter semantics or compiler semantics has nothing to do with
being an interpreted or compiled language: both Scheme interpreters
and Scheme compilers exhibit both semantics. For instance, Ikarus,
which is a native code compiler provides interpreter semantics at
the REPL whereas Ypsilon which is an interpreter, provides
compiler semantics in scripts, when the R6RS compatibility flag is set.

In general the same program in the same implementation can be run both
with interpreter semantics (when typed at the REPL) and with compiler
semantics (when used as a library), but the way the program behaves is
different, depending on the semantics used.

There is no such distinction in Python, which has only
interpreter semantics.  In Python, everything happens at runtime,
including bytecode compilation (it is true that technically bytecode
compilation is cached, but conceptually you may very well think that
every module is recompiled at runtime, when you import it - which is
actually what happens if the module has changed in the
meanwhile). Since Python has only interpreter semantics there is no
substantial difference between typing commands at the REPL and writing
a script.

Things are quite different in Scheme. The interpreter semantics is
*not specified* by the R6RS standard and it is completely
implementation-dependent. It is also compatible with the standard to
not provide interpreter semantics at all, and to not provide a REPL:
for instance PLT Scheme does not provide a REPL for R6RS programs.  On
the other hand, the compiler semantics is specified by the R6RS
standard and is used in scripts and libraries.

The two semantics are quite different. When a
program is read in interpreter semantics, everything happens at
runtime: it is possible to define a function and immediately after a
macro using that function. Each expression entered is
compiled (possibly to native code as in Ikarus) and executed
immediately. Each new definition augments the namespace of known
names at runtime, both for first class objects and macros. Macros
are also expanded at runtime.

When a program is read in compiler semantics instead, all the definitions
and the expressions are read, the macros are expanded and the program compiled,
*before* execution. Whereas an interpreter looks at a program one expression
at the time, a compiler looks at it as a whole: in particular, the order
of evaluation of expressions in a compiled program is unspecified,
unless you specify it by using a ``begin`` form.

Let me notice that in my opinion having
an unspecified evaluation order is an clear case of premature
optimization and a serious mistake, but unfortunately this is the
way it is. The rationale is that in some specific circumstances
some compiler could take advantage of the  unspecified evaluation order
to optimize the computation of some expression and run a few percent
faster but this is certainly *not* worth the complication.

Anyway, since the interpreter semantics is not specified by the R6RS
and thus very much implementation-dependent, I will focus on the
compiler semantics of Scheme programs. Such semantics is quite
tricky, especially when macros enters in the game.

.. _9: http://www.artima.com/weblogs/viewpost.jsp?thread=240804

Macros and helper functions
---------------------------------------------------------------------

You can see the problem of compiler semantics once you start using macros
which depend from auxiliary functions. For instance, consider this
simple macro

$$ASSERT-DISTINCT

which raises a compile-time exception (syntax-violation) if it is
invoked with duplicate arguments. Such macro could be used as a
helper in macros defining multiple names at the same time, like
the ``multi-define`` macro of episode 9_.
``assert-distinct`` relies on the builtin function
``free-identifier=?`` which returns true when two identifiers
are equal and false otherwise (this is a simplified explanation,
let me refer to the `R6RS document`_ for the gory details) and
on the helper function ``distinct?`` defined as follows:

$$list-utils:DISTINCT?

``distinct?`` takes a list of objects and finds out they are all
distinct according to some equality operator, of if there are duplicates.
Here are a couple of test cases:

$$TEST-DISTINCT

It is natural, when writing new code, to try things at the REPL and to
define first the function and then the macro. The problem is that
everything works in the REPL; moreover, in some Scheme implementation
like Ypsilon, the code will also
work as a script (unless the strict R6RS-compatibility flag is set).
However, in R6RS-conforming implementations, if you cut and paste from
the REPL and convert it into a script, you will run into an error!

The problem is due to the fact than in the compiler semantics macro
definitions and function definitions happens at *different times*. In
particular, macro definitions are taken in consideration *before*
function definitions, independently from their relative position in
the source code. Therefore our example fails to compile since the
``assert-distinct`` macro makes use of the ``distinct?`` function
which is *not yet defined* at the time the macro is considered,
i.e. at expansion time. Actually, functions are not evaluated
at expansion time, since functions are first class values and the right
hand side of any definition is left unevaluated by the compiler.
As we saw in the previous episode, both ``(define x (/ 1 0))`` and
``(define (f) (/ 1 0))`` (i.e. ``(define f (lambda () (/ 1 0)))``) are
compiled correctly but not evaluated until the runtime, therefore
both ``x`` and ``f`` cannot be used inside a macro.

*The only portable way to make
available at expand time a function defined at runtime is to
define the function in a different module and to import it at
expand time*

The `expansion process`_ of Scheme source code is specified in
the R6RS document. The standard has the generic concept of
*macro expansion time* which is valid even for interpreted
implementation when there is no compilation time.

Phase separation
--------------------------------------------------------------

Let me go back to the example of the ``assert-distinct`` macro.
I have put the ``distinct?`` helper function in the ``(aps list-utils)``
module, so that you can import it.  This is enough to solve the
problem of compile time vs runtime separation for Ikarus, but it is not
enough for PLT Scheme or Larceny, which have full *phase separation*.
In other words, in Ikarus (but also Ypsilon, IronScheme and Mosh)
the following script

$$assert-distinct:

is correct, but in PLT Scheme and Larceny it raises an error::

 $ plt-r6rs assert-distinct.ss 
 assert-distinct.ss:5:3: compile: unbound variable in module
 (transformer environment) in: distinct?

.. image:: salvador-dali-clock.jpg

The problem is that PLT Scheme has *full phase separation* and therefore
requires *phase specification*: by default
names defined in external modules are imported *only* at runtime, *not*
at compile time. In some sense this is absurd since
names defined in an external pre-compiled modules
are of course known at compile time
(this is why Ikarus has no trouble to import them at compile time);
nevertheless PLT Scheme (and Larceny) forces you to specify at
which phase the functions must be imported.  Notice that personally I
do not like the PLT and Larceny semantics since it makes things more
complicated, and that I prefer the Ikarus semantics:
nevertheless, if you want to write
portable code, you must use the PLT/Larceny semantics, which is the
one blessed by the R6RS document.

If you want to import a few auxiliary functions
at expansion time (the time when macros are processed; often
incorrectly used as synonymous for compilation time) you must
use the ``(for expand)`` form:

``(import (for (only (aps list-utils) distinct?) expand))``

With this import form, the script is portable in all R6RS implementations.

Discussion
-------------------------------------------------

Personally, I find the interpreter semantics the most intuitive and
easier to understand. In such semantics everything happens at runtime,
and there is no phase separation at all; it is true that the code may
still be compiled before being executed, as it happens in Ikarus, but
this is an implementation detail: from the point of view of the
programmer the feeling is the same as using an interpreter.
The interpreter semantics is also the most powerful semantics at all:
for instance, it is possible to redefine identifiers and it is
possible to import modules at runtime, things which are both impossible
in compiler semantics.

After all, if you look at it with honesty, the compiler semantics is
nothing else that a *performance hack*: by separing compilation time
from runtime you can perform some computation only once (at compilation time)
and gain performance. This is not strange at all: compilers *are*
performance hacks. It is just more efficient to convert a a program into
machine code with a compiler than to interpret one expression at the time.
Since in practice there are lots of situations where performance is
important and one does need a compiler, it makes a lot of sense to
have a compiler semantics. The compiler
semantics is also designed to make separate compilation and cross compilation
possible. Therefore the compiler semantics
has many practical advantages and
I am willing cope with it, even if it is not as
straightforward as interpreter semantics.

Moreover, there are (non-portable) tricks to define helper functions
at expand time without need to move them into a separate module, therefore
it is not so difficult to work around the restrictions of the compiler
semantics.

The thing I really dislike is full phase separation. But a full discussion
of the issues releated to phase separation will require a whole episode.
See you next week!

.. _expansion process: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-13.html#node_chap_10

|#

(import (rnrs) (sweet-macros) (for (aps list-utils) expand)
        (for (aps lang) expand run) (aps compat) (aps easy-test))

;;ASSERT-DISTINCT
(def-syntax (assert-distinct arg ...)
  #'(#f)
  (distinct? free-identifier=? #'(arg ...))
  (syntax-violation 'assert-distinct "Duplicate name" #'(arg ...)))
;;END

;(assert-distinct a b a)

;;DEF-BOOK
(def-syntax (def-book name title author)
  (: with-syntax
     name-title (identifier-append #'name "-title")
     name-author (identifier-append #'name "-author")
     #'(begin
         (define name (vector title author))
         (define name-title (vector-ref name 0))
         (define name-author (vector-ref name 1)))))

;;END
(pretty-print (syntax-expand (def-book bible "The Bible" "God")))


 ;;TEST-DEF-BOOK
 (test "def-book"
       (let ()
         (def-book bible "The Bible" "God")
         (list bible-title bible-author))
       (list "The Bible" "God"))
 ;;END

             
;;ALIST2
 (def-syntax (alist2 arg ...)
   (: with-syntax ((name value) ...) (normalize #'(arg ...))
     (if (for-all identifier? #'(name ...))
         #'(let* ((name value) ...)
             (list (list 'name name) ...))
         (syntax-violation 'alist "Found non identifier" #'(name ...)
                           (remp identifier? #'(name ...))))))
;;END

(run

 ;;TEST-DISTINCT
 (test "distinct"
       (distinct? eq? '(a b c))
       #t)
 
 (test "not-distinct"
       (distinct? eq? '(a b a))
       #f)
 ;;END
 
 (let ((a 1))
   (test "mixed"
         (alist2 a (b (* 2 a)))
         '((a 1) (b 2))))
 )

