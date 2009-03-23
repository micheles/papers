#|
===================================================================

Working around phase separation
--------------------------------------------------------------

I have always hated being force to put my helper functions in an
auxiliary module, because I often use auxiliary functions which
are intended to be used only once inside a given macro, thus
it makes sense to put those auxiliary functions *in the same
module* as the macro the are used in.
In principle you could solve the problem by definining all the
functions *inside* the macro, but I hate this, both for dogmatic
reasons (it is a Pythonista dogma that *flat is better than nested*)
and for pragmatic reasons, i.e. I want to be able to debug my
helper functions and this is impossible if they are hidden inside
the macro. They must be available at the top-level. Moreover, you
never know, and a functionality which was intended for use in a specific
macro my turn useful for another macro after all, and it is much
more convenient if the functionality is already encapsulated in
a nice exportable top level function.

I am not the only to believe that it should be possible to define
helper functions in the same module as the macro and actually
many Scheme implementations provide a way to do so via a
``define-for-syntax`` form which allows to define function
at *expand time*, so that they are available for usage in macros.

If your Scheme does not provide ``define-for-syntax``, which is not
part of the R6RS specification, you can still work around phase
separation with some clever hack. For instance, you could
use the following macro:

$$DEFINE-CT

The problem with auxiliary macros
------------------------------------------------------------------

The most common manifestation of phase separation is the problem
with auxiliary functions we have just discussed. In general however,
there is the same problem for any identifier which is used in the
right hand side of a macro definition. There is however a subtility
with auxiliary macros.

In systems with strong phase separation, like
PLT Scheme and Larceny, auxiliary macros
are not special, and they behave as auxiliary functions: you
must put them into a separare module and you must import them
with ``(for (only (module) helper-macro) expand)`` before using them.
In system with weak phase separation, like Ikarus, or without
phase separation, like Ypsilon, *there is no need to put auxiliary
macros in an external module.* The reason is that all macro
definitions are read at the same time, and the compiler knows
about the helper macros, so it can use them. Systems with
strong phase separation are effectively using different namespaces
for each phase.

Let me make an example. Suppose you wanted to define the macros
``define-ct`` and ``alist`` in the same module:

.. code-block:: scheme

 (import (rnrs) (sweet-macros))

 (def-syntax (alist arg ...)
    <code here> ...)

  (def-syntax (define-ct kw (define name value) ...)
    #'(def-syntax kw
        (let ((a (alist (name value) ...)))
             <more code here> ...)))

In Ikarus that would be perfectly possible: the first syntax
definition would add a binding fro ``alist`` to the compile time
namespace, so that it would be seen by the second syntax definition.

In PLT and Larceny, instead,
since the second ``def-syntax`` would still
see the standard R6RS environment - supplemented by the bindings
defined in ``sweet-macros`` -  and would not see the binding for
``alist``: the net result is that you would get an error,


This is a precise design choice: systems with strong phase
separation are making the life harder for programmers,
by forcing them to put auxiliary macros (and functions)
in auxiliary modules, to keep absolute control on how the
names enter in the different phases and to make possible
to use different languages at different phases.

I have yet to see a convincing example of why keeping
different languages at different phases is worth
the annoyance.

Compile time module systems versus runtime module systems
-----------------------------------------------------------------

Since the title of this series is "The Adventures of a Pythonista in
Schemeland" I have decided to begin my escursion of the R6RS module
system by contrasting it with Python module system.
Python modules are runtime objects which can be introspected
(they are basically dictionaries); Scheme modules instead are
compile time entities which are not first class objects, and cannot
be introspected. It is not difficult to implement a Python-like
module system in Scheme, by making use of hash-tables (the equivalent
of Python dictionaries): let me begin by performing this exercise,
to make clear what a runtime module system is and to contrast it
with the compile time module system than Scheme is actually using.
The trick to define a module object is to collect all the definitions
(for simplicity let me consider only ``define`` forms) into a hashtable

Implementing a first class module system
-----------------------------------------

|#
            
(import (rnrs) (sweet-macros) (for (aps lang) expand) (aps compat))
                                 
(define sentinel (gensym))

(def-syntax (hash-lambda h)
  (syntax-match ()
     (sub ())))

;; I would write the module system over alists
(define (alist->hash a)
  (define h (make-eq-hashtable))                                                
  (for-each (lambda (x) (hashtable-set! h (car x) (cadr x))) a)
  (case-lambda
    (() h)
    ((name) (hashtable-ref h name sentinel))))
 
(def-syntax (module-object def ...)
  (: with-syntax (name ...) (map get-name-from-define #'(def ...))
     #'(let ()
         def ...
         (alist->hash (list (list 'name name) ...)))))

(display (syntax-expand (module-object
                         (define a 1)
                         (define (f) a))))
                         
(define mod1
  (module-object
   (define a 1)
   (define (f) a)))

(display (mod1 'a))
(display ((mod1 'f)))

;(define mod1 (alist (a 1) (f (lambda () a))))

(define-ct example
  (define x 1)
  (define y (* x 2)))

(pretty-print (syntax-expand
(define-ct example
  (define x 1)
  (define y (* x 2)))))

(display (list (example x) (example y)))
         
