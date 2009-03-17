#|
A bit of history 
=====================================================

This is the first episode of a long running series of articles about
Scheme. Currently I have published the first 11 episodes of it on
Stacktrace. This episode is a revised translation of
http://stacktrace.it/2008/02/le-avventure-di-un-pythonista-schemeland-1/

My target
-------------------------------------------------------------------

As you can imagine from the title, this series has been written from
the point of view of a Python programmer. Nevertheless,
it should be easy to follow for any programmer familiar with
any dynamic language such as Perl, Ruby, PHP, Tcl, etc.  In
general all those languages (let me call them mainstream dynamic languages) are
similar: interpreted, very dynamic, with a strong support for
scripting (ok, maybe PHP does not fit the last point, but you get the
idea ;)

Scheme is different. Even if it is very dinamic and well
suited for scripting, very often it is also compiled (both
to native code or to a target language such as C, Java or the CLR)
and can work at C speed. Moreover Scheme is a functional
language and it makes use of a set of functional idioms which
are unknown in the mainstream languages. Finally, Scheme offers
to its user an extremely advanced macrology (actually it has the most
advanced macro system I know) and extremely powerful features (such
as continuations) without equivalent in other languages. 

That
means that learning Scheme is not trivial: actually it takes of
lot of effort and motivation to master it.
If you see a programming language just as a tool to perform a given
job in the smallest possible time, and your job is not programming language
research, then you should not learn Scheme. Scheme is for people
who want to know  the many possible ways of performing the same job,
who want to understand the advantages and the disadvantages of the
different approaches, who want to explore programming paradigms.

I do not think that the first kind of programmers (let say the
engineers) is inferior to the second kind (let say
the explorers) or viceversa. It all depends on where your interest
lies. If you are doing bioinformatics and you researching a cure
for a genetic sickness I expect you to solve your problem in
the smallest possible amount of time with a specialized library
without dispersing your efforts reinventing the wheel.
On the other hand, if you are a Computer Science professor
I would expect to you to know many different languages and
programming languages paradigms, having reinvented many wheels.

Pythonistas, generically speaking, are in between: they are pragmatic
programmers who want to do a real job, but they are also persons which
are not content with the first language they find, otherwise they
would stay for their entire life programming Visual Basic, Java or
C++. They are both engineers and explorers at the same time (you could
say that a good engineer should be a bit of an explorer, too,
especially in a fast changing field such a programming).

This series is meant for programmers that fit the description
I have just given. Its main goal is to discuss a few features of
the Scheme programming language, with the aim to solicit your
curiousity and make you think if you can learn something useful
from this language which is dismissed by most as being just an
academic language.

A bit of programming language history: Fortran e Lisp
---------------------------------------------------------------

The `history of programming`_ begins with two languages with
two completely different philosophies and goals: Fortran and Lisp.
Both languages come from Academia, but from two opposite fields:
on one side we find physicists and engineers interested in numeric
computations to be run in the most efficient way to solve concrete
problems of physics/engineering; on the other side we find
matematicians interested in algorithmic research trying to
solve abstract problems like symbolic computation, theorem proving,
artificial intelligence and related topics.

Both fields had first class brains and the result of their effort
were Fortran, which is still - after fifty years - the reference point
for numeric computation, and Lisp, which is still the reference
point for metaprogramming tecniques. Both languages had and still
have and enormous success in they market niche and will be probably
still be us one hundred years from now.
Nevertheless, both Fortran and Lisp are nowadays languages of small
visibility, since their niches has become very small and far away
from what we mean as mainstream programming today.

The reason for the little popularity of Fortran is clear: the language
has been designed with one and only one goal in mind, efficency in
numeric computation (*number crunching*). For everything else,
Fortran, is not an appealing choice. Nowadays, most programmers have
no reason to write libraries for floating point computations (they are
already written, or they are only written by specialized people) so
they have no need for Fortran. Also, C and C++ are nearly as efficient
as Fortran and they have substantial advantages from the point of view
of the interface with the operating system; moreover, most scientific
tasks nowadays involves using a variety of technology and glue
languages shine in this context: for instance you could use Python for
writing the user interface and the visualization software, by calling
underlying scientific libraries written in C or in Fortran.

The reason for the little popularity of Lisp is less clear:
Lisp (I mean here Lisp in a large sense, intending the whole
family of Lisp-derived languages including Scheme) is a general
purpose language, it could do everything, it is nearly as fast
as C, but nobody is using it. Newsgroups are full of flame wars
between people claiming that Lisp is dead versus people claiming
that Lisp is not dead at all and that it will be the language
of the future. I will prudentelly avoid all these hot debates,
I would not formulate any theory about the popularity of Lisp,
and I will just discuss the Scheme language, leaving the reader
to formulate his own opinion ;)

.. _history of programming: http://www.levenez.com/lang/history.html

The algorithmic language Scheme
-------------------------------------------------------------

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/scheme_shield.png

Scheme was born in 1975 (it is nearly twice as old as Python) as a
dialect of the Lisp family. Nowadays by "Lisp" we refer usually to the
language Common Lisp as standardized in 1989, well after Scheme. To
discuss the differences and the advantages/disadvantages of Scheme
with respect to Common Lisp would be long and I would expose myself to
flame wars: usenet is full of furious discussions between Scheme and
Lispers saying that their languages are completely different and that
the opposite language is complete crap; nevertheless, anybody not
knowning Scheme nor Lisp would have difficulty to distinguish one from
the other (!)

Basically, both languages share a lot of features and a lot of what I
will say about Scheme will apply to Common Lisp too. The biggest
differences are sociological: the Scheme community is more academic
and interested in research, experimentation and didactic; the
Common Lisp community is closer to the IT business world and
interested in solving real word problems. Of course this is a
simplification but there is some truth in it. In the past,
Scheme was meant to be a small language and it was particularly easy to
implement; nowadays, this is not true anymore, since compliance
with the latest Scheme specification requires a lot of work from
the implementor side. Many people on the Scheme community are not
happy with that, but a larger specification should in principle
improve portability between implementations. Historically,
Common Lisp was born as  *union* of may features presents in Lisp
dialects before standardization, whereas Scheme was born as
*intersection* of the same features. The
`Revised Report 5 on the Algorithmic language Scheme`_ (aka R5RS) says:

  *Programming languages should be designed not by piling feature on 
  top of feature, but by removing the weaknesses and restrictions that 
  make additional features appear necessary.* -- William Clinger

.. _Revised Report 5 on the Algorithmic language Scheme: http://www.schemers.org/Documents/Standards/R5RS/HTML/

As a consequence of this principle, all Scheme standards up to R5R6
are much smaller than the Common Lisp standard: actually too small, so
that it is practically impossible to write "real" applications
following the standard only. Recently people have tried to solve this
issue by introducing a new standard, much bigger than the previous
ones, the hotly debated R6RS ( `Revised Report 6 on the Algorithmic
language Scheme`_).  The preparation of this standard has generated
endless suffering in the Scheme community, since a significant
minority has seen it as a betrayal of the spirit of Scheme.  Nowadays
Scheme is no more a little language: the R6RS requires a module
system, a condition system, advanced macrology, a standard library,
unicode support and many other features not requested before.  Not
only it is difficult to write a new implementation, it is also
difficult to take an old R5RS implementation and to make it compatible
with the new standard. Since R6RS is relatively recent (it was
published in September 2007) there are few implementations of it. The
first were `Larceny`_ and `Ikarus`_; now there is also
`ypsilon`_. Moreover, `PLT Scheme`_ has grown an R6RS-compatibility
mode. I will use Ikarus for my examples.

.. image:: http://www.phyast.pitt.edu/~micheles/scheme/Ikarus_Scheme_Logo.png

The installation procedure is trivial, it is enough to download the
tarball and to compile with the usual ``configure`` and ``make``
dance.  You can test that your installation works by invoking the
interactive prompt::
 
 $ ikarus
 Ikarus Scheme version 0.0.3
 Copyright (c) 2006-2008 Abdulaziz Ghuloum
 
 > (display "hello world\n")
 hello world

If you are running Windows, you may want to install Common Larceny, that
runs on .NET.

This is the end: in the next episode I will discuss the problem of the
implementations of Scheme and the issue of the portability of
libraries. See you soon!

.. _Revised Report 6 on the Algorithmic language Scheme: http://www.r6rs.org/
.. _Larceny: http://www.ccs.neu.edu/home/will/Larceny/
.. _Ikarus: http://www.cs.indiana.edu/~aghuloum/ikarus/
.. _PLT Scheme: http://www.plt-scheme.org/
.. _ypsilon: http://code.google.com/p/ypsilon/
|#
