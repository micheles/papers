.. -*- mode: rst -*-

Book Proposal: Object Oriented Python
============================================

:Author: Michele Simionato
:Version: 0.1
:Date: 2009-03-13
:Status: Draft

This book focuses on the Object Oriented aspects of Python. 
However, it does not try to emphasize general topics of OOP that are
exportable to other languages. Instead, its aim is to
emphasize specific techniques which are exquisitely Pythonic.
One of the goals of this book is to enable you *to program Python in Python*
and not in Java, in C++, or in your favorite language.
To this aim, the Python object model is discussed in detail, since
it is quite different from the object model of most languages and it
provides many features which is important to know, even if you choose
to not use it. The goal of this book is to  


The text before the first heading should introduce the reader to the
aims and scope of this book in the context of current trends in the
field, current technology, and so on. What issues are important? What
compelled you to write this book?

 .. line-block::

  *There is only one way to learn: trough examples*

The Philosophy Of This Book
---------------------------

This book is written with the intent to help the programmer going trough
the fascinating concepts of Object Oriented Programming (OOP), in their
Python incarnation. Notice that I say to help, not to teach. Actually,
I do not think that a book can teach OOP or any other non-trivial matter 
in Computer Science or other disciplines. Only the
practice can teach: practice, then practice, and practice again. 
You must learn yourself from your experiments, not from the books. 
Nevertheless, books are useful. They cannot teach, but they can help. 
They should give you new ideas that you was not thinking about, they should
show tricks you do not find in the manual, and in general they should be of
some guidance in the uphill road to knowledge. That is the philosophy
of this book. For this reason

1. It is not comprehensive, not systematic; 
it is intended to give ideas and basis: from
that the reader is expected to cover the missing part on his own,
browsing the documentation, other sources and other books, and finally
the definite autority, the source itself.

2. It will not even try to teach the *best* practices. I will show what you can
do with Python, not what you "should" do. Often I will show solutions that are
not recommended. I am not a mammy saying this is
good, this is bad, do this do that.  


3. You can only learn from your failures. If you think "it should work, if I do
X and Y" and it works, then you have learned nothing new. 
You have merely verified
that your previous knowledge was correct, but you haven't create a new
knowledge. On the other hand, when you think "it should work, if I do
X and Y" and it doesn't, then you have learned that your previous knowlegde
was wrong or incomplete, and you are forced to learn something new to
overcome the difficulty. For this reason, I think it is useful to report
not only how to do something, but also to report how not to do something, 
showing the pitfalls of wrong approaches.

That's in my opinion is the goal of a good book. I don't know if have
reached this goal or not (the decision is up to the reader), but at least
I have tried to follow these guidelines.

Moreover, this is not a book on OOP, 
it is a book on OOP *in Python*. 

In other words, the point of view of this book is not 
to emphasize general topics of OOP that are exportable to other languages, 
but exactly the opposite: I want to emphasize specific techniques that one
can only use in Python, or that are difficult to translate to other 
languages. Moreover, I will not provide comparisons with other 
languages (except for the section "Why Python?" in this introduction and
in few selected other places), 
in order to keep the discussion focused. 

This choice comes from the initial motivation for this book, which was 
to fulfill a gap in the (otherwise excellent) Python documentation. 
The problem is that the available documentation still lacks an accessible 
reference of the new Python 2.2+ object-oriented features.
Since myself I have learned Python and OOP from scratch, 
I have decided to write this book in order to fill that gap and
help others. 

The emphasis in this book is not in giving 
solutions to specific problems (even if most of the recipes of this book
can easily be tailored to solve real life concrete problems), it is in 
teaching  how does it work, why it does work in some cases and why does 
not work in some other cases. Avoiding too specific problems has an
additional bonus, since it allows me to use *short* examples (the majority 
of the scripts presented here is under 20-30 lines) which I think are 
best suited to teach a new matter [#]_ . Notice, however, that whereas
the majority of the scripts in this book are short, it is also true
that they are pretty *dense*. The density is due to various reasons:

1. I am defining a lot of helper functions and classes, that are
   reused and enhanced during all the book.

2. I am doing a strong use of inheritance, therefore a script at the
   end of the book can inherits from the classes defined through all
   the book;

3. A ten line script involving metaclasses can easily perform the equivalent 
   of generating hundreds of lines of code in a language without metaclasses 
   such as Java or C++.

To my knowledge, there are no other books covering the same topics with
the same focus (be warned, however, that I haven't read so many Python 
books ;-). The two references that come closest to the present book are
the ``Python Cookbook`` by Alex Martelli and David Ascher, and
Alex Martelli's ``Python in a Nutshell``. They are quite recent books and 
therefore it covers (in much less detail) some of the 2.2 features that are 
the central topics to this book. 
However, the Cookbook reserves to OOP only one chapter and has a quite 
different philosophy from the present book, therefore there is 
practically no overlapping. Also ``Python in a Nutshell`` covers 
metaclasses in few pages, whereas half of this book is essentially
dedied to them. This means that you can read both ;-)
 

.. [#] Readers that prefer the  opposite philosophy of using longer, 
       real life-like, examples, have already the excellent "Dive into 
       Python" book http://diveintopython.org/ at their disposal. This is 
       a very good book that I certainly recommend to any (experienced) 
       Python programmer; it is also freely available (just like this ;-).
       However, the choice of arguments is quite different and there is 
       essentially no overlap between my book and "Dive into Python" 
       (therefore you can read both ;-).

Who This Book Is For
------------------------------------------------

I have tried to make this book useful to a large public of Pythonistas, 
i.e. both people with no previous experience of Object Oriented Programming
and people with experience on OOP, but unfamiliar with the most
recent Python 2.2-2.3 features (such as attribute descriptors,
metaclasses, change of the MRO in multiple inheritance, etc). 
However, this is not a book for beginners: the non-experienced reader should 
check (at least) the Internet sites http://www.python.org/newbies.com and 
http://www.awaretek.com, that provide a nice collection of resources for Python 
newbies.

These are my recommendations for the reader, according to her/his level:

1. If you are an absolute beginner, with no experience on programming,
   this book is *not* for you (yet ;-). Go to 
   http://www.python.org/doc/Newbies.html and read one of the introductive 
   texts listed there, then come back here. I recommend "How to Think Like 
   a Computer Scientist", available for free on the net (see 
   http://www.ibiblio.org/obp/thinkCSpy/); I found it useful myself when 
   I started learning Python; be warned, however, that it refers to a rather 
   old Python version. There are also excellent books 
   on the market (see http://www.awaretek.com/plf.html). 
   http://www.uselesspython.com/ is a good resource to find recensions 
   about available Python books. For free books, look at
   http://www.tcfb.com/freetechbooks/bookphyton.html .

2. If you know already (at least) another programming language, but you don't
   know Python, then this book is *not* for you (again ;-). Read the FAQ, the
   Python Tutorial and play a little with the Standard Library (all this
   material can be downloaded for free from  http://www.python.org), then
   come back here. 

3. If you have passed steps 1 and 2, and you are confortable with Python
   at the level of simple procedural programming, but have no clue about
   objects and classes, *then* this book is for you. Read this book till
   the end and your knowledge of OOP will pass from zero to a quite advanced 
   level (hopefully). Of course, you will have to play with the code in 
   this book and write a lot of code on your own, first ;-)    

4. If you are confortable with Python and you also known OOP from other
   languages or from earlier version of Python, then this book is for
   you, too: you are ready to read the more advanced chapters.

5. If you are a Python guru, then you should read the book, too. I expect
   you will find the errors and send me feedback, helping me to improve
   this book.


What This Book Covers
--------------------------------------------------

This book covers Python 3.1, which is the most recent release of Python
(actually not yet released as I write now). All the examples and the code
in this book runs under Python 3.1, with a few exceptions when talking about
backward compatibility; the code which is meant to be run under older
version of Python is explicitely marked as so. Since at the present most
Python code in existence runs on Python 2.X, the book contains an appendix
about older version of Python and how to run the examples if you are stuck
with an older Python.

How This Book Is Structured
---------------------------------------------------------------------

This book is split in two parts. Parts I, titled "The Python Object Model"
is an in deep review of the Python object model, from the basics to
advanced topics such as descriptors, multiple inheritance and metaclasses, 
The main focus of part I is to make the reader confortable with what
Python has to offer in this respect (*know your toolbox*). In particular
the differences with more static object models (say the Java or the C++
object model) and highlighted and the advantages of the dynamic features
of Python are emphasized.

Part II is titled "Putting the Python Object model to work" is a sequence
of case studies, with the intend of showing real life non-trivial 
applications of Python features (*use your toolbox*). 
There is a difference between merely *knowing* a technique and 
*being able to use* it effectively. Knowing just requires study;
using effectively requires a maturity which is only won with hard
work, lots of practice, and lots of mistakes: in a word, we call all
of that *experience*. 

Therefore, whereas Part I is more pedagogical in intent, Part II focuses
on design decisions and discussion of best practices: given two
or more possible solutions for a problem, which one is the best
solution in terms of simplicity, extensibility, maintenability, and so
on? When it is sensible to use a technique and when it is better *not*
to use it? All the discussion is based on real life case studies 
coming from my working experience.

If you are already an experienced Python coder you will find Part II
to be the most interesting one; on the other hand, if you are just
beginning with Python you should probably focus more on Part I and
read part II in a second moment, when you have become acquainted with
the language. On the other hand, everybody is free to read the book as
he/she whishes, and you find more rewarding to skip chapters and to
read sections here and however. However, I have written this book
for cover-to-cover readers, since. I am one of them myself. Actually,
the only books I do read cover-to-cover are reference
books and books which are collections of papers.

What You Need to Use This Book
--------------------------------------------------

You do not need much to use this book. Just a computer, an editor, the
latest release of Python and an open mind. All the code described in
this book is cross platform, so you should not have any trouble whatever
your platform is. Sometimes for convenience I just how to run an example
from the Linux command line since Linux is the platform I am using 
to write the book, but you can freely replace the Linux command line
with the Windows or the Mac ones, with the obvious substitutions.

How this book is written
----------------------------------------------------------------------

I am writing this book on a MacBook running Ubuntu Linux, using Emacs as
editor, reStructuredText as text format and subversion as version control
system. The content of the book is scattered in many Python files with big
docstrings. I am using a toolchain
based on docutils and Sphynx to generate the book from the Python files.
The advantage of this approach is that the entire codebase of the book
is continuously tested, and I can be confident than I am distributing correct
code. As testing frameworks I am using doctest from the standard library
and nosetest, which is not standard, but well known in the community.

.. All the code of this book is available as a public subversion repository 
.. (if the editor allows me).

Source Code
-----------------------------------------------------------------

All of the source code used in this book is
available for download at http://www.wrox.com. Once at the site,
simply locate the book’s title (either by using the Search box or by
using one of the title lists) and click the Download Code link on the
book’s detail page to obtain all the source code for the book.

Because many books have similar
titles, you may find it easiest to search by ISBN; this book’s ISBN
is 978-0-47X-XXXXX-X.  Once you download the code, just decompress it
with your favorite compression tool. Alternately, you can go to the
main Wrox code download page at
http://www.wrox.com/dynamic/books/download.aspx to see the code
available for this book and all other Wrox books.  Errata We make
every effort to ensure that there are no errors in the text or in the
code. However, no one is perfect, and mistakes do occur. If you find
an error in one of our books, like a spelling mistake or faulty piece
of code, we would be very grateful for your feedback. By sending in
errata you may save another reader hours of frustration and at the
same time you will be helping us provide even higher quality
information.  To find the errata page for this book, go to
http://www.wrox.com and locate the title using the Search box or one
of the title lists. Then, on the book details page, click the Book
Errata link. On this page you can view all errata that has been
submitted for this book and posted by Wrox editors. A complete book
list including links to each book’s errata is also available at
http://www.wrox.com/misc-pages/booklist.shtml.  If you don’t spot “your”
error on the Book Errata page, go to
http://www.wrox.com/contact/techsupport.shtml and complete the form there to
send us the error you have found. We’ll check the information and, if
appropriate, post a message to the book’s errata page and fix the
problem in subsequent editions of the book.  p2p.wrox.com For author
and peer discussion, join the P2P forums at p2p.wrox.com. The forums
are a Web-based system for you to post messages relating to Wrox books
and related technologies and interact with other readers and
technology users. The forums offer a subscription feature to e-mail
you topics of interest of your choosing when new posts are made to the
forums. Wrox authors, editors, other industry experts, and your fellow
readers are present on these forums.  At http://p2p.wrox.com you will
find a number of different forums that will help you not only as you
read this book, but also as you develop your own applications. To join
the forums, just follow these steps: 

1. Go to p2p.wrox.com and click the Register link.  

2. Read the terms of use and click Agree.

3. Complete the required information to join as well as any optional
   information you wish to provide and click Submit.  

4. You will receive an e-mail with information describing how to
   verify your account and complete the joining process.  You can read
   messages in the forums without joining P2P but in order to post your
   own messages, you must join.  Once you join, you can post new messages
   and respond to messages other users post. You can read messages at any
   time on the Web. If you would like to have new messages from a
   particular forum e-mailed to you, click the Subscribe to this Forum
   icon by the forum name in the forum listing.  For more information
   about how to use the Wrox P2P, be sure to read the P2P FAQs for
   answers to questions about how the forum software works as well as
   many common questions specific to P2P and Wrox books. To read the
   FAQs, click the FAQ link on any P2P page.

.. Sometimes Python is accused of having too many web frameworks. And
.. it's true, there are a lot. That said, I think writing a framework
.. is a useful exercise. It doesn't let you skip over too much
.. without understanding it. It removes the magic. So even if you go
.. on to use another existing framework (which I'd probably advise
.. you do), you'll be able to understand it better if you've written
.. something like it on your own. - Ian Bicking, http://pythonpaste.org/webob/do-it-yourself.html
