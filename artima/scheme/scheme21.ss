#|
``with-syntax`` and ``generate-temporaries``
-----------------------------------------------------

The R6RS standard provides a few convenient utilities to work with
macros. One of such utilities is the ``with-syntax`` form, which
allows to introduce auxiliary pattern variables into a skeleton.
``with-syntax`` is often used in conjunction with the ``generate-temporaries``
function, which returns a list of temporary identifiers.
For instance, here is ``fold`` macro
providing a nicer syntax for the ``fold-left`` and ``fold-right``
higher order functions:

$$list-utils:FOLD

In this example, for each variable ``x`` a pattern variable ``a`` is
generated with a temporary name; the temporary variable is used
as argument in the lambda function. For instance, in Ypsilon


  ``(fold left (s 0) (x in (range 3)) (y in (range 3)) (+ s x y))``

expands to

::

 (fold-left
   (lambda (s \x2E;L271 \x2E;L272)
     (let+ (x \x2E;L271) (y \x2E;L272) (+ s x y)))
   0 (range 3) (range 3))

as you can check by using ``syntax-expand``.
The temporary
names are quite arbitrary, and you will likely get different names,
since each time ``generate-temporaries`` is called, different names are
generated. ``generated-temporaries`` is perfect to generate dummy names
used as argument of functions, as seen in this example. Another typical
usage is to generate dummy names for helper functions, as shown in
the following paragraph.

A record macro
---------------------------------------------------------------

Scheme has a vector data type, which is used to manage finite sequences
with a fixed number *n* of values, known at compile time. Each element
can be accessed in O(1) time by specifying an integer index starting from
*0* to *n*, with the notation ``(vector-ref v i)``. Vectors are perfect
to implement records, since you can see a record as a vector with *n+1*
argumments, where the 0-th element specify the type of the vector
and the i-th element is the i-th field of the record.
Notice that the stardard specifies a record system, but writing a
record system based on macros is a good exercise nonetheless.
It also provides a good example of a second order macro expanding
to a macro. Here is the code:

$$DEF-RECORD

An example will make everything clear. Suppose we want to define a
``Book`` record type; we can do so by writing

``(def-record Book title author)``

which expands to::

 (begin
  (def-syntax Book
    (syntax-match
      (<new> <signature> ? title author)
      (sub (Book <new>) (syntax record-new))
      (sub (Book <signature>) (syntax '(Book title author)))
      (sub (Book ?) (syntax record?))
      (sub (Book title) (syntax \x2E;L30))
      (sub (Book author) (syntax \x2E;L31))))
  (define (record-new title author) (vector 'Book title author))
  (define (record? b) (eq? 'Book (vector-ref b 0)))
  (define (\x2E;L30 b) (assert (record? b)) (vector-ref b 1))
  (define (\x2E;L31 b) (assert (record? b)) (vector-ref b 2)))

This code defines a ``Book`` macro and a few auxiliary functions such
as ``record-new``, ``record?`` and two others with temporary names.
The ``Book`` macro allows to create new records

::

 > (define book ((Book <new>) "title" "author"))
 > book
 #(Book "title" "author")

to introspect records

::

 > ((Book ?) book)
 #t

 > (Book <signature>)
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
|#

(import (rnrs) (sweet-macros) (for (aps lang) run expand)
        (aps easy-test) (for (aps list-utils) expand) (aps compat))

;;DEF-RECORD
(def-syntax (def-record name field ...)
  (: with-syntax
     (getter ...) (generate-temporaries #'(field ...))
     (i ...) (range 1 (+ (length #'(field ...)) 1))
     #`(begin
         (def-syntax name
           (syntax-match (<new> <signature> ? field ...)
              (sub (name <new>) #'record-new)
              (sub (name <signature>) #''(name field ...))
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
         (syntax-match (<new> <signature> ? field ...)
            (sub (name <new>) #'record-new)
            (sub (name <signature>) #''(name field ...))
            (sub (name ?) #'record?)
            (sub (name field) #'getter)
            ...))))
;;END

;(def-syntax Book (record-syntax Book title author))
;(pretty-print (syntax-expand (record-syntax Book title author)))

(def-record Book title author)
(define b ((Book <new>) "T" "A"))
(display b)
(newline)
(display (Book <signature>))
(display ((Book ?) b))


(display (syntax-expand (Book title)))
(newline)
(display ((Book title) b))
(newline)

(display ((Book author) b))

