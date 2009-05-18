#|
Generating temporary identifiers
=========================================================================

In this episode I show how to introduce auxiliary identifiers in a
macro, by using the standard R6RS utility
``generate-temporaries``. As an example, I show how you can define
record types and I discuss the hygienic feature of Scheme macros.

``generate-temporaries``
-----------------------------------------------------

The R6RS standard provides a few convenient utilities to work with
macros. One of such utilities is the ``with-syntax`` form, which
allows to introduce auxiliary pattern variables into a skeleton
(a better name would have been ``let-pattern-vars``).
Let me make an example.
``with-syntax`` is often used in conjunction with the ``generate-temporaries``
function, which returns a list of temporary identifiers.

Here is an example where the temporary variable is used
as argument in the lambda function: a ``fold`` macro
providing a nicer syntax for the ``fold-left`` and ``fold-right``
higher order functions.

$$list-utils:FOLD

Notice the usage of the literals ``left`` and ``right`` to avoid
writing two separated macros, and the usage of ``in`` to enhance
readability.

In this example, for each variable ``x`` a pattern variable ``a`` is
generated with a temporary name. For instance, in Ypsilon

.. code-block:: scheme

  (fold left (s 0) (x in (range 3)) (y in (range 3)) (+ s x y))

expands to

.. code-block:: scheme

 (fold-left
   (lambda (s \x2E;L271 \x2E;L272)
     (let+ (x \x2E;L271) (y \x2E;L272) (+ s x y)))
   0 (range 3) (range 3))

as you can check by using ``syntax-expand``.  The temporary names are
quite arbitrary, and you will likely get different names, since each
time ``generate-temporaries`` is called, different names are
generated. ``generated-temporaries`` is perfect to generate dummy
names used as arguments, as seen in this example. Another typical
usage is to generate dummy names for helper functions, as shown in the
following paragraph.

A record type macro
---------------------------------------------------------------

Scheme has a vector data type, which is used to manage finite sequences
with a fixed number *n* of values, known at compile time. Each element
can be accessed in O(1) time by specifying an integer index starting from
*0* to *n*, with the notation ``(vector-ref v i)``. Vectors are perfect
to implement records, since you can see a record as a vector with *n+1*
arguments, where the 0-th element specify the type of the vector
and the i-th element is the i-th field of the record.
Notice that the stardard specifies a record system, but writing a
record system based on macros is a good exercise nonetheless.
It also provides a good example of a second order macro expanding
to a macro. Here is the code:

$$DEF-RECORD-TYPE

.. image:: vinyl-record.png

An example will make everything clear. Suppose we want to define a
``Book`` record type; we can do so by writing

``(def-record-type Book title author)``

which expands to (in Ikarus):

.. code-block:: scheme

 (begin
  (def-syntax Book
    (syntax-match (New Signature ? title author)
      (sub (Book New) #'record-new)
      (sub (Book Signature) #''(Book title author))
      (sub (Book ?) #'record?)
      (sub (Book title) #'#{title |Ivhf4sEgOry2IG%W|})
      (sub (Book author) #'#{author |r=hJyxJbHsP$j&$3|})))
  (define (record-new title author)
    (vector 'Book title author))
  (define (record? b) (eq? 'Book (vector-ref b 0)))
  (define (#{title |Ivhf4sEgOry2IG%W|} b)
    (assert (record? b))
    (vector-ref b 1))
  (define (#{author |r=hJyxJbHsP$j&$3|} b)
    (assert (record? b))
    (vector-ref b 2)))

This code defines a ``Book`` macro and a few auxiliary functions such
as ``record-new``, ``record?`` and two others with temporary names.
The temporary names are of course implementation-specific. Ikarus
here does a nice thing by prefixing them with the names coming
from the list given as argument to ``generate-temporaries``, so
that you can ascertain their origin.

The ``Book`` macro allows to create new records

::

 > (define book ((Book New) "title" "author"))
 > book
 #(Book "title" "author")

to introspect records

::

 > ((Book ?) book)
 #t

 > (Book Signature)
 (book title author)

and to retrieve the elements of a record by field name::

 > ((Book title) book)
 "title"

 > ((Book author) book)
 "author"

Since I am a fan of functional programming, I am not providing mutation
methods, so that you may regard them as immutable records (actually
they are not, since you can change them by using ``vector-set!``,
but that would be a dirty trick ;)

Notice that a record system like the one presented here features record types
which are not first class objects - since they are macros; in this respect
it is more similar to the type system of languages like SML, where types
are not objects, and very different from a type system like the Python one,
where classes are objects. Of course in Scheme you can also implement a
Python-like object system, where it is possible to create dynamic record types
at runtime and not only at compile time. You can implement it yourself, or
wait for a future episode ;)

Hygiene
---------------------------------------------------------------------

.. hygiene in R6RS: http://docs.plt-scheme.org/r6rs-lib-std/r6rs-lib-Z-H-13.html#node_sec_12.1

There is a subtle point about the ``def-record-type`` macro defined in
the previous paragraph. Such a macro introduces a lots
of auxiliary functions, such as ``record-new``, ``record?``, and an accessor
function with a temporary name for every record field.
One may expect those names
to litter the namespace: i.e., after expansion, you would expect the names
``record-new``, ``record?`` and the temporary names to be defined in
the namespaces.
Actually this is not the case: Scheme macros are *hygienic*,
and auxiliary names introduced in the macro *are not visible outside*.

.. image:: hygienic-paper-small.jpg 

This is a major difference with respect to Common Lisp macros.
The only names which enter in the namespace are the ones we put in;
in the case of ``def-record-type`` only the name of the record type (i.e.
``Book``) enters in the namespace after macro expansion. Nonetheless,
the auxiliary names are known to ``Book`` macro, and for instance
``(Book ?)`` will expand to the right ``record?`` function.

Everything works even if
in the same module you define a different record type with a different
``record?`` function: there will be no nameclashes. The reason is that
the implementation of macros takes care of distinguishing the
names in some way (it could be based on marking the names, or on
explicit renaming).

In particular, in the ``def-record-type``
macro, I should notice that I have been able to use the name ``record?``
only because is an internal name: if the macroexpansion were literal,
I would have incurred in a name clash, since ``record?`` is a builtin
name.

In general, if you are writing a library which can be imported
in an unknown environment, in absence of hygiene you could introduce
name clashes impossible to foresee in advance, and that could be solved
only by the final user, which however will likely be ignorant of how
your library works. Therefore hygiene is a very good think, since it
frees your from wondering about name clashes.

|#

(import (rnrs) (sweet-macros) (for (aps lang) run expand)
        (aps easy-test) (for (aps list-utils) run expand) (aps compat))

;;DEF-RECORD-TYPE
(def-syntax (def-record-type name field ...)
  (: with-syntax
     (getter ...) (generate-temporaries #'(field ...))
     (i ...) (range 1 (+ (length #'(field ...)) 1))
     #`(begin
         (def-syntax name
           (syntax-match (New Signature ? field ...)
              (sub (name New) #'record-new)
              (sub (name Signature) #''(name field ...))
              (sub (name ?) #'record?)
              (sub (name field) #'getter)
              ...))
         (define (record-new field ...) (vector 'name field ...))
         (define (record? b) (eq? 'name (vector-ref b 0)))
         (define (getter b) (assert (record? b)) (vector-ref b i)) ...
      )))
;;END

;;RECORD
(def-syntax (record-syntax name field ...)
  (: with-syntax
     (getter ...) (generate-temporaries #'(field ...))
     (i ...) (range 1 (+ (length #'(field ...)) 1))
     #`(let ()
         (define (record-new field ...) (vector 'name field ...))
         (define (record? b) (eq? 'name (vector-ref b 0)))
         (define (getter b) (assert (record? b)) (vector-ref b i))
         ...
         (syntax-match (New Signature ? field ...)
            (sub (name New) #'record-new)
            (sub (name Signature) #''(name field ...))
            (sub (name ?) #'record?)
            (sub (name field) #'getter)
            ...))))
;;END

           
;(def-syntax Book (record-syntax Book title author))
;(pretty-print (syntax-expand (record-syntax Book title author)))

(def-record-type Book title author)

(pretty-print (syntax-expand (def-record-type Book title author)))

(define b ((Book New) "T" "A"))
(display b)
(newline)
(display (Book Signature))
(display ((Book ?) b))


(display (syntax-expand (Book title)))
(newline)
(display ((Book title) b))
(newline)

(display ((Book author) b))

