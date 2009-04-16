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

Interpreter semantics vs compiler semantics
------------------------------------------------------------------

One of the trickiest things about Scheme, coming from Python, is
its distinction between *interpreter semantics* and *compiler semantics*.
Python has only interpreter semantics and basically everything happens
at runtime. It is true that technically bytecode compilation is cached, but
conceptually you may very well think that every module is recompiled
at runtime, when you import it (which is actually what happens if
the module has changed).

Scheme has both interpreter semantics, which is typically used at the
REPL and is *not specified* properly, and compiler semantics which is
specified by the R6RS standard and is used in scripts and libraries.
The two semantics are quite different. When a
program is read in interpreter semantics, everything happens at
runtime: it is possible to define a function and immediately after a
macro using that function. Each expression entered is
compiled (possibly to native code as in Ikarus) and executed
immediately.

When a program is read in compiler semantics instead, all the definitions
and the expressions are read, the macros are expanded and the program compiled,
*before* execution. Whereas an interpreter looks at a program one expression
at the time, a compiler looks at it as a whole: in particular, the order
of evaluation of expressions in a compiled program is unspecified,
unless you specify it by using a ``begin`` form.

Let me notice that in my opinion having
an unspecified evaluation order is an abominable case of premature
optimization and a serious mistake, but unfortunately this is the
way it is. The rationale is that in some specific circumstances
some compiler could take advantage of the  unspecified evaluation order
to optimize the computation of some expression and run a few percent
faster but this is certainly *not* worth the complication.

Anyway, since the interpreter semantics is not specified by the R6RS
and thus very much implementation-dependent, I will focus on the
compiler semantics of Scheme programs. Such semantics is quite
tricky, especially when macros enters in the game.

You can see the beginning of the problem once you start using macros
which depend from auxiliary functions. For instance, consider this
simple macro

$$ASSERT-DISTINCT

which raises a compile-time exception (syntax-violation) if it is
invoked with duplicated arguments (a typical use case where you can
use such macro is when definining specialized lambda forms).  The
macro relies on the helper function ``distinct?`` defined as follows

$$lang-utils:distinct?

and on the builtin function
``free-identifier=?`` which returns true when two identifiers
are equal and false otherwise.

If you define first the function and then the macro at the REPL
everything *seems* to work; in some
Scheme implementation, like Ypsilon, this will also work as a
script, unless the strict R6RS-compatibility flag is set.
However, in R6RS-conforming implementations, if you cut and paste the previous
lines from the REPL and convert it into a script, you will run into
an error!

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

The only portable way to make
available a function defined at runtime at compilation time is to
define the function in a different module and to import it in the
original module.

For convenience I have put ``distinct?`` in the ``(aps list-utils)``
module, so that you can import it.  This is enough to solve the
problem for Ikarus, which has no *phase separation*, but it is not
enough for PLT Scheme or Larceny, which have *phase separation*.

Phase separation
--------------------------------------------------------------

In PLT Scheme running the script raises an error::

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
nevertheless PLT Scheme and Larceny Scheme forces you to specify at
which phase the functions must be imported.  Notice that personally I
do not like the PLT and Larceny semantics since it makes things more
complicated than needed, and that I prefer the Ikarus semantics (also
used in IroScheme and Mosh): nevertheless, if you want to write
portable code, you must use the PLT/Larceny semantics, which is the
one blessed by the R6RS document.

You may think the R6RS document to be schizophrenic, since it
accepts both implementations with phase separation and without
phase separation, but using the semantics without phase separation results in
non-portable code. Here a bold decision was required to ensure
portability: to declare the PLT semantics as the only acceptable one.

De facto, the R6RS document is the result
of a compromise between the partisans of phase separation
and absence of phase separation. 

If you want to import a few auxiliary functions
at expansion time (the time when macros are processed; often
incorrectly used as synonymous for compilation time) you must
use the ``(for expand)`` form:

``(import (for (only (aps list-utils) distinct?) expand))``

With this import form, the script compiles in all R6RS implementations.

Discussion
-------------------------------------------------

Is compiler semantics and phase separation a good thing?
It is clear that for the programmer's point of view, the simplest thing
is lack of phase separation, i.e. interpreter semantics: as soon as you type
it in, an helper function is available for use in macros.
If you look at it with honesty, at the end the compiler semantics is
nothing else that a *performance hack*: by separing compilation time
from runtime you can perform some computation only once at compilation time
and gain performance. 

Therefore, if you have a compiled version of Scheme,
it makes sense to separate compilation time from runtime, and to
expand macros *before* compiling the helper functions (in absence of
phase separation, macros are still expanded before running any runtime
code, but *after* recognizing the helper functions).
Notice that Scheme has a concept of *macro expansion time* which is
valid even for interpreted implementation when there is no compilation
time. The `expansion process`_ of Scheme source code is specified in
the R6RS.

There is still the question if strong phase separation is a good thing,
or if weak phase separation (as in Ikarus) is enough. For the programmer
weak phase separation is easier, since he does not need to specify
the phase in which he wants to import names. Strong phase separation
has been introduced so that at compile time a language which is
completely different from the language you use at runtime. In particular
you could decide to use in macros a subset of the full R6RS language.

Suppose for instance you are a teacher, and you want to force your
students to write their macros using only a functional subset of Scheme.
You could then import at compile time all R6RS procedures except the
nonfunctional ones (like ``set!``) while keeping import at runtime
the whole R6RS. You could even perform the opposite, and remove ``set!``
from the runtime, but allowing it at compile time.

Therefore strong phase separation is strictly more powerful than week
phase separation, since it gives you more control. In Ikarus, when
you import a name in your module, the name is imported in all phases,
and there is nothing you can do about it.
On the other hand strong phase separation makes everything more complicated:
it is somewhat akin to the introduction of multiple namespace, because
the same name can be imported in a given phase and not in another,
and that can lead to confusion. To contain the confusion, the R6RS
documents states that *the same name cannot be used in different phases
with different meaning in the same module*.
For instance, if the identifier ``x`` is bound to the
value ``v`` at the compilation time and ``x`` is defined even
at runtime, ``x`` must be bound to ``v`` even at runtime. However, it
is possible to have ``x`` bound at runtime and not at compile time, or
viceversa. This is a compromise, since PLT Scheme in non R6RS-compliant mode
can use different bindings for the same name at different phases.

There are people in the Scheme community thinking that strong phase
separation is a mistake, and that weak phase separation is the right thing
to do. On the other side people (especially from the PLT community where
all this originated) sing the virtues of strong phase separation and say
all good things about it. I personally I have not seen a compelling
use case for strong phase separation yet.
On the other hand, I am well known for preferring simplicity over
(unneeded) power. 

.. _expansion process: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-13.html#node_chap_10

|#

(import (rnrs) (sweet-macros) (for (aps list-utils) expand)
        (for (aps lang) expand run) (aps compat) (aps easy-test))

;;ASSERT-DISTINCT
(def-syntax (assert-distinct arg ...)
  #'((void))
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
 (let ((a 1))
   (test "mixed"
         (alist2 a (b (* 2 a)))
         '((a 1) (b 2))))
 )

