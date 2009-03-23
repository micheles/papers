#|The R6RS module system
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

Compile time module systems versus runtime module systems
-----------------------------------------------------------------

Since the title of this series is "The Adventures of a Pythonista in
Schemeland" I have decided to begin my escursion of the R6RS module
system by contrasting it with Python module system.
Python modules are runtime objects which can be introspectedGFCsixZ95OaX
(they are basically dictionaries); Scheme modules instead are
compile time entities which are not first class objects, and cannot
be introspected. It is not difficult to implement a Python-like
module system in Scheme, by making use of hash-tables (the equivalent
of Python dictionaries): let me begin by performing this exercise,
to make clear what a runtime module system is and to contrast it
with the compile time module system than Scheme is actually using.
The trick to define a module object is to collect all the definitions
(for simplicity let me consider only ``define`` forms) into a hashtable

|#

(import (rnrs) (sweet-macros) (for (aps lang) expand) (aps compat))
                                                                      
(define sentinel (gensym))

(def-syntax (hash-lambda h)
  (syntax-match ()
     (sub ())))


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

