#|Phase separation
================================================

Introduction
---------------------------------------------------------

For nearly 30 years Scheme lived without a standard module system.
That lead to neverending debate and to the proliferation of dozens
of different module systems.
The situation changed with the R6RS report: now finally Scheme has
a standard module system. Problem is: after 30 years of discussion,
they still got it wrong! It will takes me six full episodes
to explain the module system and its trickiness, especially for
macro writers who want to write portable code.

.. _5: http://www.artima.com/weblogs/viewpost.jsp?thread=239699

The phase separation concept
------------------------------------------------------------------

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

You can see the beginning of the problem once you start using macros
which depend from auxiliary functions. For instance, suppose you want
to define an utility to generate identifiers to be inserted
unhygienically inside macros. A typical use case is the definition of
a bunch of identifiers with different suffixes. We can perform the
task with the following helper function:

$$lang:IDENTIFIER-APPEND

All the functions used here (``string->symbol``, ``string-append``,
``symbol->string`` work in the obvious way.
Here is a trivial example of usage of ``identifier-append`` in a
``def-book`` macro which introduces two identifiers for the fields
``title`` and ``author``:

$$DEF-BOOK

Here is a test, showing that hygiene is effectively broken and that
the identifiers ``name-title`` and ``name-author`` are really introduced
in the namespace after expansion:

$$TEST-DEF-BOOK

Everything *seems* to work, if you try this at the REPL; in some
Scheme implementation, like Ypsilon, this will also work as a
script, unless the strict R6RS-compatibility flag is set.
However, in most implementations, if you cut and paste the previous
lines from the REPL and convert it into a script, you will run into
an error!

The problem is due to *phase separation*, i.e. the fact that usually (except
for some REPLs and for some Scheme implementations) macro definitions
and function definitions happens at *different times*.

Ypsilon, as most interpreted Scheme implementations, has no phase
separation: there is no big difference between macros and functions,
which are simply recognized in the order given by their position in
the source code.  In our example the ``identifier-append`` function is
defined before the ``def-book`` macro, so it can be used in the right
hand side of the macro definition.

Life is different when you have a Scheme implementation supporting phase
separation, which means most Scheme implementations. For such implementations
macro definitions are taken in consideration
*before* function definitions, independently from their relative
position in the source code. Therefore our example fails to compile
since the ``def-book`` macro makes use of the ``identifier-append``
function which is
*not yet defined* at the time the macro is considered, i.e. at compilation
time. The only way to make available a function defined
at runtime at compilation time is to define the function in a different
module and to import it in the original module.

Notice that for convenience I have put ``identifier-append``, together
with a companion function ``identifier-prepend`` of obvious meaning in
the ``aps`` package, in the ``(aps lang)`` module.


Strong phase separation
--------------------------------------------------------------

This is enough to solve the problem for Ikarus, which has *weak phase
separation*, but it is not enough for PLT Scheme or Larceny, which have
*strong phase separation*.


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

``(import (for (only (aps lang) identifier-append) expand))``

Discussion
-------------------------------------------------

Is phase separation a good thing?
It is clear that for the programmer's point of view, the simplest thing
is lack of phase separation. This is the semantic typically (but now
always) chosen by Scheme interpreters and REPLs: as soon as you type
it in, an helper function is available for use in macros.
If you look at it with honesty, at the end phase separation is
nothing else that a *performance hack*: by separing compilation time
from runtime you can perform some computation at compilation time only
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
the phase in which he want to import names. Strong phase separation
has been introduced so that at compile time a language which is
completely different from the language you use at runtime. In particular
you could decided to use in macros a subset of the full R6RS language.

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
and that can lead to confusion.

There are people in the Scheme community thinking that strong phase
separation is a mistake, and that weak phase separation is the right thing
to do. On the other side people (especially from the PLT community where
all this originated) sing the virtues of strong phase separation and say
all good things about it. I personally I have not seen a compelling
use case for strong phase separation yet, and I would be happy is
some of my readers could give me such an example.
On the other hand, I am well known for preferring simplicity over
(unneeded) power. 

.. _expansion process: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-13.html#node_chap_10

|#

(import (rnrs) (sweet-macros) (for (aps lang) expand) (aps compat))
         
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

