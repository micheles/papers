r"""Foreword
----------------------------

I begun programming with Python in 2002, just after the release
of Python 2.2.  That release was a major overhaul of the language:
new-style classes were introduced, the way inheritance worked changed
and the builtin ``super`` was introduced. You may correctly say that I
have worked with ``super`` from the beginning; still, I never liked it
and over the years I have discovered more and more of its dark
corners. 

In 2004 I decided to write a comprehensive paper documenting
``super`` pitfalls and traps, with the goal of publishing it on the
Python web site, just as I had published my essay on multiple
inheritance and the `Method Resolution Order`_. With time the paper
grew longer and longer but I never had the feeling that I had covered
everything needed to be said: moreover I have a full time job, so I
never had the time to fully revise the paper as a whole. As a consequence,
four years have passed and the paper is still in draft status. This is
a pity, since it documents issues that people encounter and that
regularly come out on the Python newsgroups and forums.  

Keeping the draft sitting on my hard disk is doing a disservice to the
community. Still, I lack to time to finish it properly. I decided to
come out from the impasse by splitting the long paper in a series of
short blog posts, which I have the time to review properly. Moreover
people are free to post comments and corrections in case I am making
mistakes (for a post about ``super`` this is always possible). Once I
finish the series, I may integrate the corrections, put it together
again and possibly publish it as whole on the Python website.
In other words, I am trying the strategies of *divide et conquer*
and *release early, release often*. We will see how it goes.

Introduction
-------------------------------------------------------------

``super`` is Python a built-in, first introduced in Python 2.2 and
slightly improved and fixed in later versions, which is often
misunderstood by the average Python programmer. One of the reasons for
that is the poor documentation of ``super``: at the time of this
writing (August 2008) the documentation is incomplete and in some parts
misleading and even wrong. For instance, the standard documentation
(even for the new 2.6 version
http://docs.python.org/dev/library/functions.html#super) still says::

  super(type[, object-or-type])
    Return the superclass of type. If the second argument is omitted the 
    super object returned is unbound. If the second argument is an object, 
    isinstance(obj, type) must be true. If the second argument is a type, 
    issubclass(type2, type) must be true. super() only works for new-style 
    classes.

The first sentence is just plain wrong: ``super`` does not return the 
superclass. There is no such a thing as "the" superclass in a Multiple 
Inheritance (MI) world. Also, the sentence about *unbound* is misleading,
since it may easily lead the programmer to think about bound and unbound
methods, whereas it has nothing to do with that concept. 
IMNSHO ``super`` is one of the most trickiest and surprising Python 
constructs, and we absolutely needs a document to shed light on its secrets. 
The present paper is a first step in this direction: it aims to tell you 
the *truth* about ``super``. At least the amount of truth
I have discovered with my experimentations, which is certainly
not the whole truth ;)

A fair warning is in order here: this document is aimed to expert 
Pythonistas. It assumes you are familiar with `new-style classes`_
and the `Method Resolution Order`_ (MRO) 
concept; moreover a good understanding of descriptors_ would be extremely 
useful. Some parts also require good 
familiarity with metaclasses_. All in all, this paper is not for the faint 
of heart ;)

There is no superclass in a MI world
----------------------------------------------------------

Readers familiar will single inheritance languages, such as
Java or Smalltalk, will have a clear concept of superclass
in mind. This concept, however, has *no useful meaning* in Python or in
other multiple inheritance languages. I became convinced of this fact
after a discussion with Bjorn Pettersen and Alex Martelli 
on `comp.lang.python in May 2003`_
(at that time I was mistakenly thinking that one could define a
superclass concept in Python). Consider this example from that
discussion:

 ::

            +-----+
            |  T  |
            |a = 0|
            +-----+
          /         \
         /           \
     +-------+    +-------+
     |   A   |    |   B   | 
     |       |    | a = 2 |
     +-------+    +-------+
         \           /
          \         /
            +-----+
            |  C  |
            +-----+
               :
               :    instantiation
               c

 >>> class T(object):
 ...     a = 0

 >>> class A(T):
 ...     pass

 >>> class B(T):
 ...     a = 2
 
 >>> class C(A,B):
 ...     pass
 
 >>> c = C()

Which is the superclass of ``C``? There are two direct superclasses (i.e. bases)
of ``C``: ``A`` and ``B``. ``A`` comes before ``B``, so one would naturally 
think that the superclass of ``C`` is ``A``. Iif ``super(C,c)`` was returning 
the superclass of ``C``, 
then it should return ``A``. ``A`` inherits its attribute ``a`` from ``T``,
where ``a`` has the value 0, so ``super(C,c).a`` should return 0. This
is NOT what happens. Instead, ``super(C,c).a`` walks trought the
method resolution order  of the class of ``c`` (i.e. ``C``) 
and retrieves the attribute from the first class above ``C`` which
defines it. In this example the MRO of ``C`` is ``[C, A, B, T, object]``, so
``B`` is the first class above ``C`` which defines ``a`` and ``super(C,c).a``
correctly returns the value 2, not 0:

 >>> super(C,c).a
 2

You may call ``A`` the superclass of ``C``, but this is not an useful
concept since the methods are resolved by looking at the classes
in the MRO of ``C``, and not by looking at the classes in the MRO of ``A``
(which in this case is ``[A,T, object]`` and does not contain ``B``). 
The whole MRO is needed, not just the first superclass.

So, using the word *superclass* in the standard doc is completely
misleading and should be avoided altogether.

.. _Method Resolution Order: http://www.python.org/download/releases/2.3/mro/
.. _new-style classes: http://www.python.org/download/releases/2.2.3/descrintro/
.. _descriptors: http://users.rcn.com/python/download/Descriptor.htm
.. _metaclasses: http://www.ibm.com/developerworks/library/l-pymeta.html
.. _comp.lang.python in May 2003: http://tinyurl.com/5ms8lk
"""

if __name__ == '__main__':
    import doctest; doctest.testmod()
