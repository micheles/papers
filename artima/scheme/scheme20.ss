#|The compilation and evaluation strategy of Scheme programs
=======================================================================

One of the trickiest aspects of Scheme, coming from Python, is its
distinction between *interpreter semantics* and *compiler semantics*.

The problem is that the same program can be executed both with
interpreter semantics (typically when typed at the REPL)
and with compiler semantics (typically when run as a script), but the way the
program behaves is different. Moreover, there
are programs which are valid at the REPL but are rejected by the compiler.

To make things worse, the interpreter semantics is unspecified by the
R6RS report, whereas the compiler semantics is loosely specified,
so that there are at least three different and incompatible semantics
about how programs are compiled and libraries are imported:
the Ikarus/Ypsilon/IronScheme/MoshScheme one, the Larceny one and the
PLT one. 

In other words, there is no hope of making programs with the
interpreter semantics portable; moreover, there also plenty of
programs with compiler semantics which are not portable. 

Fortunately the module system works well enough for most simple
cases. The proof is that we introduced the R6RS module system in
episode 5_, and for 15 episode we could go on safely by just using the
basic import/export syntax. However, once nontrivial macros enters in
the game, things are not easy anymore.

.. _5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699
.. _R6RS document: http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-13.html#node_idx_1142
.. _discussed in the previous article: http://www.artima.com/weblogs/viewpost.jsp?thread=251476
.. _expansion process: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-13.html#node_chap_10

Interpreter semantics vs compiler semantics
------------------------------------------------------------------

First of all, let me clarify what I do mean by interpreter semantics
and compiler semantics, terms which have nothing to do with
being an interpreted or compiled language, since both Scheme interpreters
and Scheme compilers exhibit both semantics.

*Compiler semantics* means that a program has (at least)
two phases, the run-time phase and the expand-time phase, and
some parts of the programs are executed at expand-time and some
other parts of the program are executed at run-time. Scheme has
a generic concept of *macro expansion time* which is valid even
for interpreted implementation when there is no compilation time.

*Interpreter semantics* means that a program is fully evaluated at
runtime, with no distinction between phases (for pure interpreters)
or with interleaved expansion and evaluation phases (for
incremental compilers).

For instance Ikarus and Ypsilon work as incremental compilers
at the REPL (I consider this as interpreter semantics, by stretching
the terminology) and as batch compilers for scripts (for Ypsilon
this is true only when the R6RS
compatibility flag is set).

Python works as an incremental compiler at the REPL (each time you enter
a function in the REPL it is compiled to bytecode, and you can
extract the bytecode by looking at ``.func_code`` attribute) and
as batch compiler for scripts.

Conceptually, in Python everything happens at runtime, including
bytecode compilation. While technically bytecode compilation
is cached, conceptually you may very well think that every module
is recompiled at runtime, when you import it - which is actually what
happens if the module has changed in the meanwhile.

In short, you can consider Python as an interpreter (as it is usually
done) and there is no substantial difference between typing commands
at the REPL and writing a script. There are a few minor differences
actually, but they are not relevant for what I am discussing now.

.. image:: Interpreter_Symbol.jpg

Things are quite different in Scheme. The interpreter semantics is
*not specified* by the R6RS standard and it is completely
implementation-dependent. It is also compatible with the standard to
not provide interpreter semantics at all, i.e. to not provide a REPL:
for instance PLT Scheme does not provide a REPL for R6RS programs
(it does provide a REPL for non R6RS programs which is actually quite
exceptional since it uses compiler semantics and not interpreter
semantics!).

The compiler semantics i.e. the `expansion process`_
of Scheme source code is (loosely) specified by the R6RS
standard and is used in libraries. The semantics used in scripts is not clear
(in the words of Will Clinger *there is no such thing as an R6RS-conforming
Scheme script, because Scheme scripts are described only
by a non-binding document that was never ratified*).

.. _9: http://www.artima.com/weblogs/viewpost.jsp?thread=240804

The difference between the two semantics is most visible when you have
macros depending on helper functions. When a program is read in
interpreter semantics, everything happens at runtime: it is possible
to define a function and immediately after a macro using that
function.

When a program is read in batch compiler semantics instead, *all* the
definitions and the expressions are read, the macros are expanded and
the program compiled, *before* execution.

Implementations have a considerable freedom in what they allowed
to do; for instance Ypsilon scripts use batch compiler semantics when the
``--r6rs`` flag is set, but by default they use incremental
compiler semantics, just as the REPL. On the opposite side of the
spectrum, the PLT REPL (in non-R6RS mode) basically
uses batch compiler semantics.

In any case the behavior of code typed the REPL is never identical to
the behavior of a script: for instance, at the REPL you can import
modules at any moment, whereas in a script you must import them at the
beginning.  There are other subtler differences, for instance in the
behavior of continuations. Then bottom line is that you should not
believe your REPL blindly.

Macros and helper functions
---------------------------------------------------------------------

As I said, you see the problem of compiler semantics once you
start using macros which depend from auxiliary functions. More in
general there is the same problem for any identifier which is used in
the right hand side of a macro definition and not inside the
templates. For instance, consider this simple macro

$$ASSERT-DISTINCT

which raises a compile-time exception (syntax-violation) if it is
invoked with duplicate arguments. Such macro could be used as a
helper in macros defining multiple names at the same time, like
the ``multi-define`` macro of episode 9_.
``assert-distinct`` relies on the builtin function
``bound-identifier=?`` which returns true when two identifiers
are equal and false otherwise (this is an extremely simplified explanation,
let me refer to the `R6RS document`_ for the gory details) and
on the helper function ``distinct?`` defined as follows:

$$list-utils:DISTINCT?

``distinct?`` takes a list of objects and finds out they are all
distinct according to some equality operator, of if there are duplicates.
Here are a couple of test cases:

$$TEST-DISTINCT

It is natural, when writing new code, to try things at the REPL and to
define first the function and then the macro. The problem is that the
code will work in REPL: however, in R6RS-conforming implementations,
if you cut and paste from the REPL and convert it into a script, you
will run into an error!

The explanation is that in compiler semantics macro
definitions and function definitions happens at *different times*. In
particular, macro definitions are taken in consideration *before*
function definitions, independently from their relative position in
the source code. Therefore our example fails to compile since the
``assert-distinct`` macro makes use of the ``distinct?`` function
which is *not yet defined* at the time the macro is considered,
i.e. at expansion time. Actually, not only functions are not evaluated
at expansion time and cannot be used inside a macro, but in general
the right hand side of any definition is left unevaluated by the compiler.
This explains why ``(define x (/ 1 0))`` is compiled correctly,
as we `discussed in the previous article`_ .

There are nonportable ways to avoiding writing the helper
functions in a separate module. For instance Ypsilon scripts by
default (unless the strict R6RS-compatibility flag is set) use
interpreter semantics and have no phase separation. On the other
end of the spectrum, mzscheme has very strong phase separation,
but it is still possible to define helper functions at expand-time
without putting them in a separated module, using the nonportable
``define-for-syntax`` form.

Nevertheless, *the only portable way to make
available at expand time a function defined at runtime is to
define the function in a different module and to import it at
expand time*.

A note about incremental compilers and interpreters
------------------------------------------------------------------

Ikarus and Ypsilon use the semantics of an *incremental compiler*:
each top level block of code is compiled - to native code in Ikarus and
to bytecode in Ypsilon - and executed immediately.  Each new definition
augments the namespace of known names at runtime, both for first class
objects and macros. Macros are both defined and expanded at runtime.

It is clear tha the semantics of an incremental compiler is
very similar to the semantics of an interpreter; here is an example in
Ikarus, where a macro is defined which depends from a helper function:

.. code-block:: scheme

 > (define (double x) (* 2 x)) 
 > (def-syntax (m) (double 1))
 (m)
 2

However, an incremental compiler is not identical to an interpreter,
since internally it uses phase separation to compile blocks
of code; for instance in Ikarus if you put together the
previous definition in a single block you get an error,
since the function ``double`` is known at expand-time
but not at runtime:

.. code-block:: scheme

 > (let () (define (double x) (* 2 x)) (def-syntax (m) (double 1)) (m))
 Unhandled exception
  Condition components:
    1. &who: double
    2. &message: "identifier out of context"
    3. &syntax:
        form: double
        subform: #f
    4. &trace: #<syntax double>

There are still Scheme implementations which are pure interpreters and
do not distinguish expand time from runtime at all; here is an example
in Guile (notice that Guile is *not* an R6RS implementation):

.. code-block:: scheme

 guile> (let () (define (double x) (* 2 x)) (define-macro (m) (double 1)) (m))
 2

I am using ``define-macro`` here which is the built-in macro mechanism
for Guile: as you see the function ``double`` is immediately available
to the macro, even if it is defined inside the same block as the macro,
which is not the case for any of the existing R6RS implementations.
Notice however that Guile also supports high level macros (via psyntax)
using compiler semantics.

Discussion
-------------------------------------------------

The interpreter semantics is the most intuitive and easier to
understand. In such semantics everything happens at runtime; the code
may still be compiled before being executed, as in incremental
compiler, but this is an implementation detail: from the point of view
of the programmer the feeling is the same as using an interpreter -
modulo the tricky point mentioned in the previous paragraph.

The interpreter semantics is also the most powerful semantics of all:
for instance, it is possible to redefine identifiers and to import
modules at runtime, things which are both impossible in compiler
semantics.

If you look at it with honesty, the compiler semantics is basically a
performance hack: by separing compilation time from runtime you can
perform some computation only once (at compilation time) and gain
performance. This is not strange at all: compilers *are* performance
hacks. It is just more efficient to convert a a program into machine
code with a compiler than to interpret it expression by expression.

The other main reason to favor compilers over interpreters, apart
from performance, is compile-time cheching. Compilers are able to reject a
class of incorrect programs even before executing them.
Scheme compilers are traditionally not too strong in this respect, because of
dynamic typing and because of the design philosophy of the
language (be permissive, we will solve the errors later). Nevertheless,
with macros you can in principle add all the compile-time checkings
you want (we just saw the checking for distinct names):
it is even possible to turn Scheme into a typed language, like `Typed Scheme`_.

Another (minor) advantage of the compiler semantics is that it makes
it easier for static tools to work with a program.
For instance in Python an IDE cannot implement autocompletion of names in a
reliable way, without having knowledge of the running program. In Scheme
an IDE can statically determine all the names imported by the program
and thus offer full autocompletion.

.. _Typed Scheme: http://www.ccs.neu.edu/home/samth/typed-scheme/
|#

(import (rnrs) (sweet-macros) (for (aps list-utils) expand)
        (for (aps lang) expand run) (aps compat) (aps easy-test))

;;ASSERT-DISTINCT
(def-syntax (assert-distinct arg ...)
  #'(#f)
  (distinct? bound-identifier=? #'(arg ...))
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

