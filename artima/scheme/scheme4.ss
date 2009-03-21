#|
Scheme bibliography (and a first program)
===========================================================

Scheme resources for beginners
-----------------------------------------------------------------

The present series has the ambition to be a nearly self-consistent
complement to the R6RS document. In theory you should be able
to learn Scheme by reading my "Adventures" and the R6RS 
only. In practice, however, having a look at other sources
cannot hurt, so I will discuss here a few tutorials/textbooks
you can find on the net and which are useful for Scheme beginners.
There are many good texts, but none focused
on my target of readers, i.e. experienced programmers coming
from the scripting language world.
I myself am the kind of persons that prefer learning from tutorials,
articles and newsgroups more than from books, therefore
I am not an expert on the existing bibiliografy. Here I will
cite only a few resources, readers more knowledgeable are
invited to post their recommendations as comments.

.. image:: books.jpg
 :width: 176

The main reference I used to learn Scheme when I started is
`Teach Yourself Scheme in Fixnum Days`_ by Dorai Sitaram, 
which has many good things going for it. It is a self-consistent
tutorial on Scheme which is very well written, especially for the first part.
It is very informative, but at the same time concise and readable
by hobbyist Scheme programmers like myself, i.e. by people using another language
at work and not having too much free time at their disposal
(I suppose that description fits the majority of my readers).
On the other hand, `Teach Yourself Scheme in Fixnum Days`_ 
is a bit old as a reference, and it completely ignores the modern
Scheme macrology, based on pattern matching. It only describes the
traditional macrology based on ``define-macro``, which many seasoned
Schemers do not love. My aim in this series is to give a description
of modern Scheme, updated to the R6RS document; moreover I want to
discuss in detail modern macros.

A more modern text (not covering the R6RS specification anyway)
is `The Scheme Programming Language`_ (Third Edition) by
R. Kent Dybvig. This is a very good book on Scheme in general.
It is also the best reference I have read on ``syntax-case`` macros, 
but it is book with several hundreds of dense pages, perhaps
too much for a hobbyist Schemer.

A very recent book is `Programming
Languages Application and Interpretation`_, by Shriram Krishnamurthi,
which is also excellent, especially the part about continuations,
but also very demanding from the reader, since it is a textbook
for university students. Notice that even this book does not cover
R6RS (to my knowledge there are no text books covering the R6RS standard,
since it is too recent).  

There is a habit of denoting Scheme books with their initials, so the
two books I have just cited are also known as TSPL3 e PLAI; however,
the most famous acronymous is certainly SICP, i.e. `Structure and
Interpretation of Computer Programs`_, by Harold Abelson e Gerald Jay
Sussman. This book has been used to teach Scheme to generations of
students and it is considered a *cult*, but I personally do not know
it, therefore I cannot comment. Another book I have not read but I
have heard good things about is `How to Design Programs`_ by
Fellesein, Findler, Flatt and Krishnamurthi, which is a textbook for
first year college students. It is up to you to check it and to see if
you like it.

As you see, there are plenty of Scheme books, being Scheme a language
with a great academical tradition. The problem is not the lack of
books, is the lack of time to read them! This is one of the reasons
why my *Adventures* are appearing as blog posts and not as a book:
a short paper of 5-6 pages is much less scary than a big book
of 500-600 pages. Morevoer blog posts are allowed to keep a much more
informal tone than books, so they are both easier to write and to read.

.. _Structure and Interpretation of Computer Programs: http://mitpress.mit.edu/sicp/full-text/book/book.html
.. _Programming Languages Application and Interpretation: http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/2007-04-26/ 
.. _Teach Yourself Scheme in Fixnum Days: http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-1.html
.. _The Scheme Programming Language: http://www.scheme.com/tspl3/
.. _How to Design Programs: http://www.htdp.org/2003-09-26/Book/

A simple Scheme program
----------------------------------------------------------------------

After so much talk, let me show you (finally!) 
a small example of Scheme program.
There is a long tradition of giving the factional function as an
example and I do not see a reason to break the tradition.
Here is the Scheme code::

  ;; fac.scm for Chicken Scheme
  (define (fac x)
     (if (= x 0) 1
      (* x
       (fac (- x 1)))))

  (define n (string->number (car (reverse (argv)))))
  (display (fac n))

.. image:: exclamation.jpg

The equivalent in Python would be::

  import sys

  def fac(x):
    if x == 0:
      return 1
    else:
      return x * fac(x-1)

  n = int(sys.argv[-1])
  print fac(n),

This trivial example already proves what I have been saying all along:

1. 
   There are lots of parenthesis: five parens at the end of the
   factorial and four at the end of the definition of ``n``. A
   typical program contains 3-4 parens per line. It should be
   noticed that all those parens are useless. By using the SRFI-49
   the code could have been written as

   ::

    define fac 
       if (= x 0) 1
        * x
         fac (- x 1)

    define n 
     string->number 
      car (reverse argv)
    display (fac n)

2. 
   The script is fully non-portable; to my knowledge
   it only works in  Chicken Scheme.
   The reason is that the R5RS standard DOES NOT SPECIFY any way
   to read the command line arguments, hence ``argv`` is not
   standard.

   To a Pythonista such a lack looks absurd, but it is only after
   thirty years that the Schemers have decided how to manage
   ``sys.argv`` in the R6RS standard, which however is still little
   diffused and probably will remain a minority standard for years
   to come.

3. 
   To get the last element of ``argv``, Python uses the
   standard syntax ``argv[-1]``; there is no standard function syntax to do it
   in Scheme, therefore or you use a non-portable function, or you reverse
   the list and you keep the first element with ``car``
   (if you want to know the origin of the term you may have a look
   at this `Wikipedia article`_): this is not really readable,
   but readability never counted much in the Scheme world.
   Some Scheme implementations
   accepts the more readable name ``first`` as a synonimous of
   ``car``, but this is again not standard.

4. 
   The result of ``fac`` depends on the implementation: some
   implementations support infinite precision numbers (this is
   required by the R6RS) but some implementations do not.
   In particular in Chicken one gets

::

 $ rlwrap csi
 CHICKEN
 Version 2.732 - macosx-unix-gnu-x86     [ manyargs dload ptables applyhook cross ]
 (c)2000-2007 Felix L. Winkelmann        compiled 2007-11-01 on michele-mac.local (Darwin)
 #;1> (define (fac x)  (if (= x 0) 1 (* x (fac (- x 1)))))
 #;2> (fac 10)
 3628800
 #;3> (fac 100)
 9.33262154439441e+157
 #;4> (fac 1000)
 +inf

In Ikarus (which is *R6RS-compliant*) one gets instead::

 $ rlwrap ikarus
 Ikarus Scheme version 0.0.2
 Copyright (c) 2006-2007 Abdulaziz Ghuloum
 > (define (fac (x) (if (= x 0) 1 (* x (fac (- x 1))))))
 > (fac 10)
 3628800
 > (fac 100)
 93326215443944152681699238856266700490715968
 264381621468592963895217599993229915608941463
 976156518286253697920827223758251185210916864
 000000000000000000000000
 > (fac 1000)
 4023872600 ... < many many other digits>

After reading these first episodes you may be tempted to quit;
I am sure the readers who followed me up to this point had this
question floating in their minds: *is it really worth it?*.
Probably for most readers the answer is *no*. But this series is
for the most persistent readers, and I hope to show them something
positive in the next episode. Keep reading and see you next time!

.. _Wikipedia article:  http://en.wikipedia.org/wiki/CAR_and_CDR
|#