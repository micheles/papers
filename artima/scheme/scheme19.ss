#|Recursive macros
=====================================================================

After a short introduction about the relevance of macros for
programming language design, I show a few common patterns of Scheme
macrology: recursive macros, accumulators, and the usage of literals
to incorporate helpers in macros.

Should everybody be designing her own programming language?
------------------------------------------------------------------

Macros are the main reason why I first became interested in Scheme. At
the time - more or less six years ago - there was a bunch of lispers
trolling in comp.lang.python, and arguing for the addition of macros
to Python.  Of course most Pythonista opposed the idea, but at the
time I had no idea of the advantages/disadvantges of macros; I felt
quite ignorant and powerless to argue. I never liked to feel ignorant,
so I decided to learn macros, especially Scheme macros, since they are
the state of the art.

Nowadays I can say that the addition of macros to Python would be
a bad idea knowing what I am talking about. Actually, I have already
stated in episode 12_ an even stronger opinion, i.e. that macros
are more bad than good for any enterprise-oriented language (but notice
that I am *not* implying that every enterprise should adopt
only enterprise-oriented languages).

My opinion against macro in (most) enterprise programming
does mean that macros are worthless, and indeed
I think they are extremely useful and important in another domain,
i.e. in the domain of design and research about programming
languages. As a matter of fact, *Scheme macros enable every programmer
to write her own programming language*. I think this is a valuable and nice
to have thing. Everybody who has got opinion
about language design, or about how an object should should work, or
questions like "what would a language look like if it had feature X?",
can solve his doubts by implementing the feature with macros.

Perhaps not everybody should design its own programming language,
and certainly not everybody should *distribute* its own personal
language, however I think lots of people will get a benifit trying
to think about how to design a language and making some experiment.
The easier thing is to start from a Domain Specific Language (DSL),
which does not need to be a full grown programming language; for
instance in the Python world it seems that everybody is implementing templating
languages to generate web pages. In my opinion, this a good thing *per se*,
the probably is that everybody is distributing its own language so that
there a bit of anarchy, but this is not such a serious problem after all.

Even for what concerns full grown programming languages we see nowadays
an explosion of new languages coming out, especially for the Java and
the CLR platforms, since it is relatively easy to implement a new
language on those platforms. However, it still takes a lot of work.

On the other hand, writing a custom language embedded in Scheme by
means of macros is by far much easier and Scheme makes an ideal platform
for implementing languages and experimenting with new ideas.

There is a `quote of Ian Bicking`_ about Web frameworks which struck me:

*Sometimes Python is accused of having too many web frameworks. And
it's true, there are a lot. That said, I think writing a framework is
a useful exercise. It doesn’t let you skip over too much without
understanding it. It removes the magic. So even if you go on to use
another existing framework (which I'd probably advise you do), you'
ll be able to understand it better if you've written something like it
on your own.*

You can the replace the words "web framework" with "programming
language" here and the quote still makes sense. You should read my
*Adventures* in this spirit: the goal of this series is to give
the technical competence to write your own language by means of
macros. Even if you are not going to design your own language,
macros will help you to understand how languages work.

I personally am interested only in the
technical competence, *I do not want to write a new language*.
There are already lots of languages
out there, and writing a real language is a lot of grunt work, because
it means writing debugging tools, good error messages, wondering about
portability, interacting with an user community, et cetera et cetera.
Not everybody is good language designer and a good BDFL, for sure;
however everybody can have opinions about language design and some
experiment with macrology can help to put to test such opinions.

.. _quote of Ian Bicking: http://pythonpaste.org/webob/do-it-yourself.html
.. _12: http://www.artima.com/weblogs/viewpost.jsp?thread=240836

Recursive macros with accumulators
----------------------------------------------------------

The goal of learning macros well enough to implement a programming language
is an ambitious one; it is not something we can attain in an episode of the
Adventures, nor in six. However, one episode is enough to explain at least
one useful tecniques which is commonly used in Scheme macrology and which
is good to know in order to reach our final goal, in time.
The technique we will discuss in this episode is the accumulator trick,
which is analogous to the accumulator trick we first discussed in episode
6_ when talking about tail call optimization. In Scheme it is common
to introduce an auxiliary variable to store a value which is passed
in a loop: the same trick can be used in macros, at compile time instead
that at run time.

In order to give an example of usage of the accumulator trick, let me
define a conditional macro ``cond-`` which works like ``cond``, but
with less parenthesis::

 (cond-
    cond-1? return-1
    cond-2? return-2
        ...
    else return-default)

We want the code above to expand to::

 (cond 
   (cond-1? return-1)
   (cond-2? return-2)
        ...
   (else return-default))


Here is the solution, which makes use of an accumulator and of an auxiliary
macro:

$$COND-

The code should be clear. The auxiliary (private) macro ``cond-aux``
is recursive: it works by collecting the arguments ``x1, x2, ..., xn``
in the accumulator ``(acc ...)``. If the number of arguments is even,
at some point we end up having collected all the arguments in the
accumulator, which is then expanded into a standard conditional; if
the number of arguments is even, at some point we end up having
collected all the arguments except one, and a ``"Mismatched pairs"``
exception is raised. The user-visible macro ``cond-`` just calls
``cond-aux`` by setting the initial value of the accumulator to ``()``.
The entire expansion and error checking is made at compile time.
Here is an example of usage::

 > (let ((n 1))  
     (cond- (= n 1) ; missing a clause
       (= n 2) 'two
       (= n 3) 'three
       else 'unknown))
 Unhandled exception:
 Condition components:
   1. &who: cond-
   2. &message: "Mismatched pairs"
   3. &syntax:
       form: (((= n 1) (= n 2)) ('two (= n 3)) ('three else) 'unknown)
       subform: 'unknown

A trick to avoid auxiliary macros
----------------------------------------------------------------

I have nothing against auxiliary macros, however sometimes you may
want to keep all the code in a single macro. This is useful if you are
debugging a macro since an auxiliary macro is usually not exported and
you may not have access to it without changing the source code of the
module defining it and without recompiling it; on the other hand, you
have full access to an exported macro including the features of the
would be auxiliary macro. The trick is to introduce a literal to
defined the helper macro inside the main macro. Here is how it would
work in our example:

$$COND2

If you do not want to use a literal identifier, you can use a literal string
instead:

$$COND3

This kind of tricks are quite common in Scheme macros; the best reference
you can find detailing these technique and others is the `Syntax-Rules Primer
for the Merely Eccentric`_, by Joe Marshall. The title is a play on the essay
`An Advanced Syntax-Rules Primer for the Mildly Insane`_ by
Al Petrofsky. 

.. image:: mad-scientist.jpg

Marshall's essay is quite nontrivial, and it is intended for expert
Scheme programmers. On the other hand, it is child play compared to
Petrofsky's essay, which is intended for Scheme wizards with a vein
of foolinesh ;)

.. _An Advanced Syntax-Rules Primer for the Mildly Insane: http://groups.google.com/group/comp.lang.scheme/browse_frm/thread/86c338837de3a020/eb6cc6e11775b619?#eb6cc6e11775b619
.. _6: http://www.artima.com/weblogs/viewpost.jsp?thread=240198

.. _Syntax-Rules Primer for the Merely Eccentric: http://www.xs4all.nl/~hipster/lib/scheme/gauche/define-syntax-primer.txt

|#

(import (rnrs) (sweet-macros))

;;COND-
(def-syntax cond-aux 
  (syntax-match ()
   (sub (cond-aux (acc ...))
        #'(cond acc ...))
   (sub (cond-aux (acc ...) x1)
     #'(syntax-violation 'cond- "Mismatched pairs" '(acc ... x1) 'x1))
   (sub (cond-aux (acc ...) x1 x2 x3 ...)
     #'(cond-aux (acc ... (x1 x2)) x3 ...))
   ))

(def-syntax (cond- x1 x2 ...)
  (cond-aux () x1 x2 ...))
;;END


;;COND3
 (define-syntax cond- 
  (syntax-match () 
   (sub (cond- "aux" (acc ...))
     (cond acc ...))  
   (sub (cond- "aux" (acc ...) x)
     (syntax-violation 'cond- "Mismatched pairs" '(acc ... x) 'x))
   (sub (cond- "aux" (acc ...) x1 x2 x3 ...)
     (cond- "aux" (acc ... (x1 x2)) x3 ...))  
   (sub (cond- x1 x2 ...)
     (cond- "aux" () x1 x2 ...))))
;;END

;;COND2
 (define-syntax cond- 
  (syntax-match (aux) 
   (sub (cond- aux (acc ...))
     (cond acc ...))  
   (sub (cond- aux (acc ...) x1)
     (syntax-violation 'cond- "Mismatched pairs" '(acc ... x1) 'x1))
   (sub (cond- aux (acc ...) x1 x2 x3 ...)
     (cond- aux (acc ... (x1 x2)) x3 ...))  
   (sub (cond- x1 x2 ...)
     (cond- aux () x1 x2 ...))))
;;END
