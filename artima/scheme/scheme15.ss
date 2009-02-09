#|
In my *Adventures* I have talked many times of pattern matching,
but only in the context of *compile time* pattern matching in macros.
There is another form of pattern matching,
which is quite common in Scheme and in other
functional languages: *run time* pattern matching (actually common
functional languages such as SML, Haskell or even Scala
only have run time pattern matching,
since they lack macros). It is impossible to overvalue
the importance of pattern matching which is in my
opinion one of the most important concepts in programming.
Unfortunately, this technique is only available in ultra high
level programming languages and therefore it is usually unknown to
the average programmer. I saw pattern matching for the
first time in '95, when using Mathematica for High Energy Physics
symbolic computations. Nowadays, the situation is changing: the
trend toward higher and higher abstraction is influencing all
programmming languages and I am pretty sure than soon or later
pattern matching will enter in mainstream languages. For the moment,
you can find it in functional languages and, in a poor man form,
in certain scripting languages.

Pattern matching in Python and in Scheme
----------------------------------------------------

Python has some support for pattern matching in the form of
tuple unpacking (note for lispers: you would call it destructuring bind).
For instance, you can write::

 >>> (a, b) = (1, 2)
 >>> (a, b)
 (1, 2)

or

::

 >>> (a, (b, [c, d])) = (1, [2, iter((3, 4))])
 >>> (a, b, c, d)
 (1, 2, 3, 4)

Tuple unpacking works at any level of nesting and for any kind of
iterable, therefore it is pretty powerful. Moreover, tuple unpacking
is even more powerful in Python 3.0, where it is possible to
split an iterable into its head (``car``) and tail (``cdr``)::

 >>> head, *tail=(i for i in (1,2,3))
 >>> (head, tail)
 (1, [2, 3])

I have already noticed, when discussing the notation for functions with a
variable number of arguments (variadic functions),
that the star syntax ``*`` in Python is similar to the dot syntax
``.`` in Scheme;
this syntactic extension in Python 3.0 makes the similarity
stronger.

.. _match: http://citeseer.ist.psu.edu/rd/34737315%2C53980%2C1%2C0.25%2CDownload/http://citeseer.ist.psu.edu/cache/papers/cs/4091/ftp:zSzzSzftp.cs.rice.eduzSzpubliczSzlanguageszSzwrightzSzmatch.pdf/wright95pattern.pdf

Scheme has very good support for compile time pattern matching,
but there is no *standard* library for runtime pattern matching
(as always :-(). Anyway, there are plenty of libraries
for runtime pattern matching: one of the most common is the
match_ library by Andrew Wright, which is available more or less
for all Scheme implementation. In Chicken Scheme match_ is actually
built-in in the core language::

 $ csi
 CHICKEN
 Version 2.732 - macosx-unix-gnu-x86     [ manyargs dload ptables applyhook cross ]
 (c)2000-2007 Felix L. Winkelmann        compiled 2007-11-01 on michele-mac.local (Darwin)

 #;1> (match-define (head . tail) '(1 2 3))
 #;2> (list head tail)
 (1 (2 3))

The main difference between Python and Scheme is that Scheme
pattern matching is not polymorphic, i.e. you cannot match
with the same pattern a list and a vector or an equivalent
iterable. You must use different patterns, or esplicitely convert
the types.

Recently the implementation of match has been rejuvenated by Alex Shinn, who
fixed a few bugs and reimplemented everything in terms of hygienic macros
(I mean ``syntax-rules`` macros, whereas the original used ``define-macro``):
this modern
implementation is also available as an R6RS library, thanks to
Derick Eddington and you can download it for here_, if you want to
use this matcher with Ikarus.

Studying the documentation of match_ is certainly a good use of your
time, and a recommended reading; on the other hand, writing your
own matcher relying on Scheme macros is even more interesting.
This will by the main goal of this episode. In the next paragraph
I will implement a ``let+`` macro with the full power of
tuple unpacking, and future episodes I will implement a full fledged
list matcher.

.. _documentazione del modulo match: http://chicken.wiki.br/Pattern%20matching
.. _librerie per il pattern matching: http://schemecookbook.org/view/Cookbook/PatternMatchingChapter
.. _here: http://bazaar.launchpad.net/~derick-eddington/ikarus-libraries/xitomatl/annotate/121?file_id=asmatch.sls-20080507230024-prlxlzdsg0x0ad3d-1

A list destructuring binding form (let+)
------------------------------------------------------

In this paragraph I will introduce a ``let+`` syntax to perform
unpacking/destructuring of lists. I will use a test-first approach,
by starting with a specification of how
``let+`` is intended to work by means of tests (I am using here the
minimal testing framework I have introduced in `episode #11`_):

$$TESTS

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/pattern-matching.jpg

Here is an implementation satisfying those tests:

$$list-utils:LET+

It is not difficult to understand how the macro works by using
``syntax-expand``; for instance, ``let+`` with a single argument expands
as follows::

 > (syntax-expand (let+ (x) '(1) x))
 (let ((ls '(1)))
   (if (null? ls)
      (apply error 'let+ "Not enough elements" '(x))
      (let+ x (car ls) (let+ () (cdr ls) x))))

whereas ``let+`` with a required argument and a variadic list of arguments
``rest`` expands as::

 > (syntax-expand (let+ (x . rest) '(1) (cons x rest)))
 (let ((ls '(1)))
   (if (null? ls)
       (apply error 'let+ "Not enough elements" '(x))
       (let+ x (car ls) (let+ rest (cdr ls) (cons x rest)))))

Notice that in this case the template ``(arg2 ... . rest)``
has been replaced by ``rest``, since there are no arguments. This is the
magic of the dots! ;)

Finally, let us see what happens when we try to match a too short list::

 > (let+ (x y) '(1) x)
 Unhandled exception
  Condition components:
    1. &error
    2. &who: let+
    3. &message: "Missing arguments"
    4. &irritants: (y)
    

or a too long list::

 > (let+ (x y) '(1 2 3) x)
 Unhandled exception
  Condition components:
    1. &error
    2. &who: let+
    3. &message: "Too many elements"
    4. &irritants: (3)

In the first case there an  argument ``(y)`` in excess, not matched by any
element; in the second case, there is an element ``(3)`` in excess,
not matched by any argument. The implementation also checks (at compile time)
that the passed arguments are valid identifiers::

 > (let+ (x y 3) '(1 2 3) x)
 Unhandled exception
  Condition components:
    1. &who: let+
    2. &message: "Argument is not an identifier"
    3. &syntax:
        form: 3
        subform: #f
    4. &trace: #<syntax 3>

As I said, Scheme pattern matching is not polymorphic: you cannot
exchange a vector for a list of viceversa::


 > (let+ (x (y z)) (list 1 (vector 2 3)) (list x y z))
 Unhandled exception:
  Condition components:
    1. &assertion
    2. &who: car
    3. &message: "argument does not have required pair structure"
    4. &irritants: (#(2 3))

The error message is clear, we also know that a ``car`` was involved,
but unfortunately, it does not give
much information about *where* exactly the error happened :-(
I was serious at the end of `episode #12`_, when I said that
debugging macros is no fun: the problem is that the errors
happen in expanded code which is invisible to the programmer.

.. _episode #11: http://www.artima.com/weblogs/viewpost.jsp?thread=240833
.. _episode #12: http://www.artima.com/weblogs/viewpost.jsp?thread=240836

|#

(import (rnrs) (aps test-utils) (aps list-utils) (sweet-macros))


(run
;;TESTS
 (test "name value"
  (let+ x 1 x); locally bind the name x to the value 1 and return it
  1)
 
 (test "no args"
  (let+ () '() 1); no bindings; return 1
  1)
 
 (test "one arg"
  (let+ (x) '(1) x); locally bind the name x to the value 1 and return it
  1)
  
 (test "two args"
  (let+ (x y) (list 1 2) (list x y)); locally bind the names x and y
  '(1 2))

 (test "pair"
   (let+ (x . y) '(1 2) y)
   '(2))
 
 (test "nested"
   (let+ (x (y z)) '(1 (2 3)) (list x y z)); bind x, y and z
   '(1 2 3))
;;END
 )
