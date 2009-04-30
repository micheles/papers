#|The tower of metalevels
=============================================

This episode goes down in dept through the Dark Secrets and Black Magic
of Scheme implementations with strong phase separation and their
undetermined tower of metalevels.

The Dark Tower
---------------------------------------------------------------------

As I explained in the previous episode, even if you do not like explicit
phasing you must to understand it in order to write portable programs.
In order to truly understand explicit phasing, you must
reason in terms of a (Dark) Tower of import levels, or
meta-levels_. Metalevels are just phases;
we have already encountered two metalevels: the
runtime phase (metalevel 0) and expand time phase (metalevel 1).

Actually the
forms ``(for (lib) run)`` and ``(for (lib) expand)`` are just
shortcuts for ``(for (lib) (meta 0))`` and ``(for (lib) (meta 1))``,
respectively. However, the full tower of metalevels is arbitrarily
high (!) and extends in two directions, both for positive and for
negative integers.

Since the publication of the Aristotle's Metaphysics,
the word *meta* has been associated to arcane and difficult matters.
The concept of meta-level is no exception to the rule ;)

.. figure:: DarkTower.jpg

   Aziz facing the dark tower of metalevels

You can find a full description of the tower of metalevels in the R6RS
document, in a rather dense paragraph in section 7. There is also
a celebrated paper by Matthew Flatt, `You want it when`_ which
predates the R6RS by many years and describes the module system
used by PLT Scheme.

Here I will try to show a concrete example of a macro
where it is essential to understand the meta-level concept.

My example macro is a compile time ``name -> value`` mapping, with some
introspection feature:

$$experimental/ct-mapping:

This is a kind of second order macro, since it is a macro which
expands to a macro transformer; its usage is obvious in
                              ; implementations with implicit
phasing:

$$use-ct-mapping:

If you run this script in Ikarus or Ypsilon
you will get the following unsurprising result::

 $ ikarus --r6rs-script use-ct-mapping.ikarus.ss 
 Available colors: (red green yellow)(R G Y)

However, in PLT Scheme, the above will fail with a very cryptic error message::

 $ plt-r6rs use-ct-mapping.ss
 /home/micheles/.plt-scheme/4.0/collects/experimental/ct-mapping.sls:8:25:
 compile: bad syntax; reference to top-level identifier is not allowed,
 because no #%top syntax transformer is bound in: quote

I was baffled by this error, so I asked for help in the PLT mailing
list, and I got a very enlightning answer from Matthew Flatt.

The tricky point here is that there is nothing wrong with
the client script, and there is no way to fix the problem by editing
it: the problem is in the library code! However, the problem is hidden
(you can compile the library without issues) and
you see it only when you use the client code, not when you compile
the library. Also, the fix is pure
dark magic: you need to rewrite the import
code in ``(experimental ct-mapping)`` by replacing

  ``(import (rnrs))``

with

  ``(import (rnrs) (for (rnrs) (meta -1))``

i.e. the ``ct-mapping`` macro must import the ``(rnrs)`` environment
at metalevel -1! Why it is so? and what the hell is metalevel -1?

Metalevels
---------------------------------------------------------------------

The concept of metalevel is only relevant in macro programming.  When
you define a macro, the right hand side of the definition can only
refer to names which are one metalevel up, i.e. typically at metalevel
1 (expand time).  On the other hand, inside a template one goes back
one metalevel, and therefore usually a template expands at metalevel
0 (runtime).

However, in the case of ``ct-mapping`` macro, the
template is itself a ``syntax-match`` form, and therefore the
templates of this inner ``syntax-match`` expand one level down, at
metalevel -1.  This is why the macro needs to import the ``(rnrs)``
bindings at metalevel -1 and why the error message says that ``quote``
is unknown.

(def-syntax ct-mapping                           ;; metalevel 0
  (let ()                                        ;; metalevel 1
    <code here ...>                              ;; metalevel 1
  (syntax-match ()
   (sub (ct-mapping (name value) ...)            ;; metalevel 0
        #'(syntax-match (<names> name ...)       ;; metalevel 0
            (sub (ctx <names>)                   ;; metalevel -1
                 #''(name ...))                  ;; metalevel -1
            (sub (ctx name)                      ;; metalevel -1
                 #'value)                        ;; metalevel -1
            ...))))

Actually ``quote`` is the only needed binding, so it would be enough
import it with the syntax ``(for (only (rnrs) quote) (meta -1))``. If
we ignored the introspection feature, i.e. we commented out the line

``(sub (ctx <names>) #''(name ...))``

there would be no need to import ``quote`` at metalevel -1, and the macro
would work without us even suspecting the existence of negative metalevels.

The metalevel tower is theoretically unbound in the
negative direction, since you can nest macro transformers at any level
of depth, and each level decreases the metalevel by one unity; on the
other hand, the tower is theoretically unbound even in the positive direction,
since a macro can have in its right hand side a macro definition which
right hand side will requires bindings defined at an higher level, and
so on. Macro definitions *increase* the metalevel; macro templates
*decrease* the metalevel. However, I suggest you not to think much
about the metalevel tower, if you don't want to risk your head ;)

.. image:: exploding-head.jpg

Here is an example of metalevel 2:

(def-syntax (m1)
  (let-syntax()
    (def-syntax (m2)
      (begin (display "Metalevel 2")
             #'42)))
  (m2))


Things are even trickier: if we keep the line
``(sub (ctx <names>) #''(name ...))`` in the original macro, but we
do not use it in client code, the original macro will apparently work,
and will break at the first attempt of using the introspection
feature, with an error message pointing to the problem in client code,
but not in library code!

All this trouble is missing in Ikarus and Ypsilon, where
importing a module once imports it for *all* metalevels.
In Ikarus and Ypsilon, all metalevels (or phases) share to the same
language and this is why I say they are weakly separated. This remark
should also make clear why many people (including myself) prefer
weak phase separation to strong phase separation.

.. _meta-levels: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-10.html#node_sec_7.2


Here is the skeleton of a Scheme program in implementations with explicit
phasing, with comments marking the different meta-levels.

.. code-block:: scheme

 (library (example-lib)         ;; expand time
 (import (rnrs) (sweet-macros)) ;; expand time/run time
 (export x y z)                 ;; expand time
 (define x some-expr)           ;; run time
 (def-syntax y                  ;; expand time
   (let ()                      ;; expand time
     code-here ...              ;; expand time
     (syntax-match ()          
       (sub (y)
        (let ()                 ;; expand time
          code-here ...         ;; expand time
         #'(hello)))            ;; run time
 (define z (y ...))             ;; run time
 body ...                       ;; run time
 )

Here is the explanation.

1. First the compiler recognizes that we are going to define a library
   named ``example-lib``.
2. The compiler imports both the standard R6RS names and the names
   exported in ``sweet-macros``; in Ikarus and other implementation such
   names are imported and made available both at run time and expand time;
   since this is implementation-dependent behavior I will discuss it
   further later on.
3. The compiler keeps track of the name that we are going to export.
4. The compiler sees a regular definition for the name ``x``, but it
   does not evaluate the right hand side, i.e. ``same-expr`` which
   is evalued only at runtime.
5. The compiler sees a macro definition for the names ``y``; it evaluates
   its right hand side immediately, at expand time.
6. The template in the macro transformer is evalued after the macro
   expansion, at runtime not a expand time;
7. Other definitions and the body of the library are evaluated only
   at runtime.
  
In the words of Abdul Aziz Ghuloum, the implementer of Ikarus:

  In definitions context (e.g., top-level program, library top-level,
  internal definitions), the compiler processes the body forms one by
  one looking for definitions.  If it encounters a define-syntax, the
  right-hand-side is expanded and evaluated and made available for
  expanding subsequent forms from that point on.  If it encounters a
  macro use, it expands the macro, splices it in place of the input form
  and resumes with the resulting body forms.  If it encounters a
  variable definition, it does NOT expand the right-hand-side
  expression, instead, it queues it until all definitions (variables and
  macros) are found.  At that point, the body expressions and
  right-hand-side expressions of variable definitions (that were queued)
  are expanded.
  At any rate, macro definitions *are* expanded and evaluated to
  obtain the transformer procedure *before* expansion of the rest of
  the definitions resumes.  This is true for all R6RS implementations.

Strong vs weak phase separation
---------------------------------------------------

The power of strong phase separation with phase specification is that
the language used at at compile time (the language seen by a piece of
code is the sum of the imported names) can be completely different
from the language used at runtime. In particular you could decide to
use in macros a subset of the full R6RS language.

Suppose for instance you are a teacher, and you want to force your
students to write their macros using only a functional subset of Scheme.
You could then import at compile time all R6RS procedures except the
nonfunctional ones (like ``set!``) while importing at runtime
the whole of R6RS. You could even perform the opposite, and remove ``set!``
from the runtime, but allowing it at compile time.


Clearly for the programmer weak phase separation is easier, since he
does not need to specify the phase in which he wants to import names.
On the other hand strong phase separation makes everything more
complicated: it is somewhat akin to the introduction of multiple
namespace, because the same name can be imported in a given
phase (level) and not in another, and that can lead to confusion. In
particular PLT Scheme in non R6RS-compliant mode can use different
bindings for the same name at different phases.


For instance, PLT multiple instantiation semantics has the
advantage of being consistent with separate compilation.


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
