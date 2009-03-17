#|
Simple second order macros
-------------------------------------------------------------

There is not upper limit to the level of sophistication you can reach
with macros: in particular it is possible to define higher order
macros, i.e. macros taking other macros as arguments or macros
expanding to other macros. Higher order macros allows an extremely
elegant programming style; on the other hand, they are exposed to the
risk of making the code incomprehensible and very hard to debug.
In this episode we will give a couple of examples of second order
macros taking other macros as argument.

Our first example is a generalization of the accumulator trick we
used last week to define the ``cond-`` macro. We will define a
``collecting-pairs`` macro, which as input another macro and a
sequence of arguments, and calls the input macro with the arguments
grouped in pairs.
Here is the code:

$$COLLECTING-PAIRS

``collecting-pairs`` can be used with many syntactic expressions like
``cond``, ``case``, ``syntax-rules``, et cetera. Here is an example
with the case_ expression::

 > (collecting-pairs (case 1)
       (1) 'one
       (2) 'two
       (3) 'three
       else 'unknown))
 one

Our second example if is a ``:`` macro defined as follows:

$$lang:COLON

The colon macro expects as argument another macro, the
``let-form``, which can be any binding macro such that
``(let-form ((patt value)) expr)`` is a valid syntax. For instance
``(let ((name value)) expr)`` can be rewritten as ``(: let name value
... expr)``, by removing four parenthesis. The latest version of the
``aps`` package provides a colon form in the ``(aps lang)`` module.


.. _case: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_384
..  _Arc: http://www.paulgraham.com/arcll1.html

A two-level syntax
-------------------------------------------------------------

Parens-haters may want to use ``collecting-pairs`` and the colon macro
to avoid parenthesis. They may even go further, and rant that the
basic Scheme syntax should require less parenthesis, since for
most programmers it is easier to write code with less parenthesis.
However, the Scheme philosophy favors automatic code generation
over manual writing. For instance, when writing macros, it is much easier
to use a conditional with more parenthesis like ``cond`` than a
conditional with less parenthesis like ``cond-``. The parenthesis
allows you to group expressions in group that can be repeated via
the ellipsis symbol; in practice, you can writing things like
``(cond (cnd? do-this ...) ...)`` which cannot be written
with ``cond-``.

On the other hand, different languages adopt different philosophies;
for instance Paul Graham's Arc_ uses less parenthesis. This is
possible since it does not provide a macro system based on
pattern matching (which is a big *minus* in my opinion). Is it possible
to have both? A syntax with few parenthesis for writing code manually
and a syntax with many parenthesi for writing macros. The answer is yes:
the price to pay is to double the constructs of the language and to
use a Python-like approach.

Python is a perfect example of language with a two-level syntax: a
simple syntax, limited but able to cover the most common case, and a
fully fledged syntax, giving all the power which is needed, which
however should be used only rarely. The best designed programming
language I know is Python. While not perfect, Python takes full
advantage of the two-level syntax idea. For instance

====================    =================================
Simplified syntax       Full syntax          
====================    =================================
obj.attr                getattr(obj, 'attr')
x + y                   x.__add__(y)
c = C()                 c = C.__new__(C); c.__init__()
====================    =================================

In the case of the conditional syntax, in principle we could have
a fully parenthesised ``__cond__`` syntax for usage in macros and
``cond`` syntax with less parens for manual usage. That, in theory:
in practice Scheme only provides the low level syntax, leaving to
the final user the freedom (and the burden) of implementing his
own preferred high level syntax.

.. fatto sia per motivi politici (è un linguaggio disegnato da un
.. comitato, è impossibile accordarsi su una sintassi di alto livello che
.. piaccia a tutti) che ideologici (alla maggior parte dei programmatori
.. Scheme va bene così, non amano le imposizioni).

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

;;COLLECTING-PAIRS
(def-syntax collecting-pairs
  (syntax-match ()
    (sub (collecting-pairs (name arg ...) x1 x2 ...)
     #'(collecting-pairs "helper" (name arg ...) () x1 x2 ...))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...))
     #'(name arg ... acc ...))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...) x)
     #'(syntax-violation 'name "Mismatched pairs" '(name arg ... acc ... x) 'x))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...) x1 x2 x3 ...)
     #'(collecting-pairs "helper" (name arg ...) (acc ... (x1 x2)) x3 ...))
    ))
;;END

;;TEST-COLON
(run
 (test "ok"
       (: let* x 1 y x (+ x y))
       2)
;  (test "err"
;     (catch-error (: let* x 1 y x z (+ x y)))
;      "Odd number of arguments")
 )

;;END

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

