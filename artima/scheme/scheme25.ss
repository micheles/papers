#|Macros, again
=====================================================================

This is episode #25, the beginning of another block
of six posts constituting part V of my Adventures.
Part I was an introduction to the language; part II was
an introduction to macros; part III was an introduction to functional
programming and part IV an introduction to the module system.
We are done with the introductions now, and we are ready to face
more beefy matters. In particular part V main is concerned with
expert-level macrology.

In the following six episodes I will
discuss various techniques and patterns which are commonly
used by Scheme macro writers, such as recursive macros, higher order
macros, and more. I will go more in detail
about how macros work, explaining what a syntax object is, what
a (non-)hygienic macro is and how you can to compare lexical
identifiers in macros.

I will also discuss the relation
between my own ``sweet-macros`` and traditional macros (``syntax-rules``,
``syntax-case`` and ``define-macro``) so that you should be able to
program with any Scheme macrology out there. 

Finally I will give some nontrivial example of macro, such as a
macro implementing runtime pattern matching for lists, by filling the
gap left in episode 15_ .

.. _homoiconicity: http://en.wikipedia.org/wiki/Homoiconicity
.. _15: http://www.artima.com/weblogs/viewpost.jsp?thread=249681
.. _MetaPython: http://metapython.org/
.. _Logix: http://www.livelogix.net
.. _Dylan: http://en.wikipedia.org/wiki/Dylan_programming_language

Macros pros and contras
------------------------------------------------------------------

Macros are the reason why I first became interested in Scheme, five or
six years ago. At the time - as at any time - there was a bunch of
people trolling in comp.lang.python, arguing for the addition of macros
to the language.  Of course most Pythonistas opposed the proposal.

However, at the time I had no idea of the advantages/disadvantages of
the proposal and I felt quite ignorant and powerless to argue. Since I
never liked to feel ignorant, I decided to learn macros, especially
Scheme macros, because they are the state of the art for what concerns
the subject.

Nowadays I can say that the addition of macros to Python would be
a bad idea knowing what I am talking about. I have two main objections,
one technical (and less important) and one political (the more important).

The technical reason is that I do not believe in macros in languages
without S-expressions. There are plenty of examples of macros for
languages without S-expression - for instance Dylan_ in the Lisp
world, or Logix_ and MetaPython_ in the Python world, but none of
them ever convinced me. Scheme macros are much better because of
the homoiconicity_ of the language ("homoiconicity" is just a big word for
the *code is data* concept).

I have already stated in episode 12_ my political objection, i.e. my
belief that macros are not a good fit for enterprise-oriented
languages. Of course, I am *not* implying that every enterprise should
adopt only enterprise-oriented languages; it is a matter of fact that
various cutting edge enterprises are taking advantage of
non-conventional and/or research-oriented languages, but I see them as
exceptions to the general rule.

In my day to day work (I use Python exclusively there)
I had occasion to write both small declarative
languages and small command-oriented languages, but they were so simple
that I did not feel the need for Scheme macros. Actually, judging
from my past experience, I think extremely unlikely that I will
ever need something as sophisticated as Scheme macros in my
daily work. This is why my opinion is against macros in (most)
enterprise programming.

Having said that, I do not think that macros are worthless, and actually
I think they are extremely useful and important in another domain,
i.e. in the domain of design and research about programming
languages.

Of course Scheme is not the only language where you can experiment
with language design, it is just the best language for this kind of
tasks.

A few months ago I have described
some experiment I did with the Python meta object protocol, in
order to change how the object system work, and replace
multiple inheritance with traits_. I am interested with this
kind of experiments, even if I will never use them in
production code, and I use Scheme in preference for such purposes.

.. _traits:

Writing your own programming language
----------------------------------------

The major interest of Scheme macros for me lies in the fact that they
*enable every programmer to write her own programming language*.
I think this is a valuable thing. Everybody who has got opinions
about language design, or about how an object system should should work, or
questions like "what would a language look like if it had feature X?",
can solve his doubts by implementing the feature with macros.

Notice that I recognize that perhaps not everybody should design its
own programming language, and certainly not everybody should
*distribute* its own personal language. Nevertheless, I think
everybody can have opinions about language design and some experiment
with macrology can help to put to test such opinions and to learn
something.

The easiest approach is to start from a Domain Specific
Language (DSL), which does not need to be a fully grown programming
language.

As a matter of fact, it seems that in the Python world
everybody is implementing his own templating language to generate web
pages. In my opinion, this a good thing *per se*, the problem is that
everybody is distributing his own language so that there is a bit of
anarchy, but this is not such a serious problem after all.

Even for what concerns fully grown programming languages we see nowadays
an explosion of new languages, especially for the Java and
the .NET platforms, since it is relatively easy to implement a new
language there. However, it still takes a substantial amount of work.

On the other hand, writing a custom language embedded in Scheme by
means of macros is much easier. I see Scheme as an excellent platform
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
language" and the quote still makes sense. You should read my
*Adventures* in this spirit: the ambition of this series is to give to
my readers the technical competence to write their own Scheme-embedded
languages by means of macros. Even if you are not going to design your
own language, macros will help you to understand how languages work.

Personally I am interested in the technical competence, *I do not want
to write a new language*.  There are already lots of languages out
there, and writing a real language is a lot of grunt work, because it
means writing debugging tools, good error messages, wondering about
portability, interacting with an user community, et cetera et cetera.

.. _quote of Ian Bicking: http://pythonpaste.org/webob/do-it-yourself.html
.. _12: http://www.artima.com/weblogs/viewpost.jsp?thread=240836

Recursive macros with accumulators
----------------------------------------------------------

The goal of learning macros well enough to implement a programming
language is an ambitious one; it is not something I can attain in one
episode of the Adventures, nor in six. However, one episode is enough
to explain at least one useful technique which is commonly used in
Scheme macrology and which is good to know in order to reach our final
goal, in time.

The technique I will discuss in this episode is writing recursive
macros with accumulators. In Scheme it is common to introduce an
auxiliary variable to store a value which is passed in a loop - we
discussed it in episode 6_ when talking about tail call optimization:
the same trick can be used in macros, at expand-time instead that at
run-time.

.. image:: scarab.png

In order to give an example I will define a macro *cond minus*
(``cond-``) which works like ``cond``, but with less parenthesis:

.. code-block:: scheme

 (cond-
    cond-1? return-1
    cond-2? return-2
        ...
    else return-default)

I want the code above to expand to:

.. code-block:: scheme

 (cond 
   (cond-1? return-1)
   (cond-2? return-2)
        ...
   (else return-default))


Here is the solution, which makes use of an accumulator and of an auxiliary
macro ``cond-aux``:

$$COND-

The code above should be clear. The auxiliary macro ``cond-aux``
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
debugging a macro since an auxiliary macro is usually not
exported. The trick is to introduce a literal to defined the helper
macro inside the main macro. Here is how it would work in this example:

$$COND2

If you do not want to use a literal identifier, you can use a literal string
instead:

$$COND3

These tricks are quite common in Scheme macros: we may even call them
design patterns. In my opinion the best reference
detailing these technique and others is the `Syntax-Rules Primer
for the Merely Eccentric`_, by Joe Marshall. The title is a play on the essay
`An Advanced Syntax-Rules Primer for the Mildly Insane`_ by
Al Petrofsky. 

.. image:: mad-scientist.jpg

Marshall's essay is quite nontrivial, and it is intended for expert
Scheme programmers. On the other hand, it is child play compared to
Petrofsky's essay, which is intended for foolish Scheme wizards ;)

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
