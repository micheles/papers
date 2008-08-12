r"""Foreword
----------------------------

I begun programming with Python in 2002, just after the release of
Python 2.2.  That release was a major overhaul of the language:
new-style classes were introduced, the way inheritance worked changed
and the builtin ``super`` was introduced. Therefore, you may correctly
say that I have worked with ``super`` right from the beginning; still, I
never liked it and over the years I have discovered more and more of
its dark corners.

In 2004 I decided to write a comprehensive paper documenting
``super`` pitfalls and traps, with the goal of publishing it on the
Python web site, just as I had published my essay on multiple
inheritance and the `Method Resolution Order`_. With time the paper
grew longer and longer but I never had the feeling that I had covered
everything I needed to say: moreover I have a full time job, so I
never had the time to fully revise the paper as a whole. As a consequence,
four years have passed and the paper is still in draft status. This is
a pity, since it documents issues that people encounter and that
regularly come out on the Python newsgroups and forums.  

Keeping the draft sitting on my hard disk is doing a disservice to the
community. Still, I lack to time to finish it properly. To
come out from the impasse, I decided to split the long paper in a series of
short blog posts, which I do have the time to review properly. Moreover
people are free to post comments and corrections in case I am making
mistakes (speaking about ``super`` this is always possible). Once I
finish the series, I may integrate the corrections, put it together
again and possibly publish it as whole on the Python website.
In other words, in order to finish the task,
I am trying the strategies of *divide et conquer*
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
superclass. There is no such a thing as *the superclass* in a Multiple 
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
Pythonistas. It assumes you are familiar with `new-style classes`_ and
the `Method Resolution Order`_ (MRO); moreover a good understanding of
descriptors_ would be extremely useful. Some parts also require good
familiarity with metaclasses_. All in all, this paper is not for the
faint of heart ;)

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

What is the superclass of ``C``? There are two direct superclasses (i.e. bases)
of ``C``: ``A`` and ``B``. ``A`` comes before ``B``, so one would naturally 
think that the superclass of ``C`` is ``A``. However,
``A`` inherits its attribute ``a`` from ``T``
with value ``a=0``: if ``super(C,c)`` was returning 
the superclass of ``C``, then ``super(C,c).a`` would return 0. This
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

So, using the word *superclass* in the standard docs is
misleading and should be avoided altogether.

Bound and unbound (super) methods
----------------------------------------------------------------

Having established that ``super`` cannot return the
mythical superclass, we may ask ourselves what the hell is returning 
;) The truth is that ``super`` returns proxy objects.

Informally speaking, a proxy is an object with
the ability to dispatch to the methods of other objects via delegation.
Technically, ``super`` is a class overriding the ``__getattribute__`` 
method. Instances of ``super`` are proxy objects providing 
access to the methods in the MRO. The dispatch is done in such a way
that

``super(cls, instance-or-subclass).method(*args, **kw)``

corresponds more or less to

``right-method-in-the-MRO-applied-to(instance-or-subclass, *args, **kw)``

There is a caveat at this point: the second argument can be
an instance of the first argument, or a subclass of it.
In the first case we expect a *bound* method to be returned
and in the second case and *unbound* method to be returned.
This is true in recent versions of Python: for instance, in this example

 >>> class B(object):
 ...     def __repr__(self):
 ...         return "<instance of %s>" % self.__class__.__name__

 >>> class C(B):
 ...     pass

 >>> class D(C):
 ...     pass

 >>> d = D()

you get

 >>> print super(C, d).__repr__
 <bound method D.__repr__ of <instance of D>>

and 

 >>> print super(C, D).__repr__
 <unbound method D.__repr__>

However, if you are still using Python 2.2 (there are unlucky people forced
to use old versions) your should be aware that ``super`` had a bug
and ``super(<class>, <subclass>).method`` returned a *bound* method,
not an unbound one::

 >> print super(C, D).__repr__ # in Python 2.2
 <bound method D.__repr__ of <class '__main__.D'>>

That means that in Python 2.2 you get::

 >> print super(C, D).__repr__() # in Python 2.2
 <instance of type>

``D``, seen as an instance of the (meta)class ``type``, is being passed as
first argument to ``__repr__``. 
This has been fixed in Python 2.3+, where you correctly get
a ``TypeError``:

 >>> print super(C, D).__repr__() # the same as B.__repr__()
 Traceback (most recent call last):
  ...
 TypeError: unbound method __repr__() must be called with D instance as first argument (got nothing instead)

The point is subtle, but usually one does not see problems since typically
``super`` is invoked on instances, not on subclasses, and in this case it
works correctly in all Python versions:

 >>> print super(C, d).__repr__()
 <instance of D>

When I was using Python 2.2, due to the bug just discussed, and due to
the ``super`` docstring

>>> print super.__doc__
super(type) -> unbound super object
super(type, obj) -> bound super object; requires isinstance(obj, type)
super(type, type2) -> bound super object; requires issubclass(type2, type)
Typical use to call a cooperative superclass method:
class C(B):
    def meth(self, arg):
        super(C, self).meth(arg)

I got the impression that in order to get unbound methods I needed to use
the unbound ``super`` object. This is actually untrue. To understand how 
bound/unbound methods works we need to talk about descriptors.

``super`` and descriptors
----------------------------------------------------

Descriptors (more properly I should speak of the descriptor protocol) were 
introduced in Python 2.2 by Guido van Rossum. Their primary motivation 
was technical, since they were needed to implement the new-style object 
system. Descriptors were also used to introduce new standard concepts in 
Python, such as classmethods, staticmethods and properties. Moreover, 
according to the traditional transparency policy of Python, descriptors 
were exposed to the application programmer, giving him/her the freedom
to write custom descriptors.  Any serious Python programmer should have 
a look at descriptors: luckily they are now very well documented (which was
not the case when I first studied them :-/) thanks to the `beautiful essay`_
of Raimond Hettinger. You should read it before continuing this article, 
since it explains all the details. However, for the sake of our discussion
of ``super``, it is enough to say that a *descriptor class* is just a
regular new-style class which implements a ``.__get__`` method with
signature ``__get__(self, obj, objtyp=None)``. A *descriptor object*
is just an instance of a descriptor class. 

Descriptor objects are intended to be used as attributes (hence their
complete name attribute descriptors). Suppose that ``descr`` is a
given descriptor object used as attribute of a given class C.
Then the syntax ``C.descr`` is actually interpreted by Python as a 
call to ``descr.__get__(None, C)``, whereas the same syntax for an 
instance of C corresponds to a call to ``descr.__get__(c, type(c))``.

Since the combination of descriptors and super is so tricky, the core
developers got it wrong in different versions of Python.  For
instance, in Python 2.2 the only way to get the unboud method
``__repr__`` is via the descriptor API::

 >> super(C, d).__repr__.__get__(None, D) # Python 2.2
 <unbound method D.__repr__>

You may check that it works correctly::

 >> print _(d)
 <instance of D>

In Python 2.3 one can get the unbond method by using the ``super(cls, subcls)``
syntax, but the syntax ``super(C, d).__repr__.__get__(None, D)`` also
works; in Python 2.4+ instead the same syntax returns a *bound* method,
not an unbound one:

>>> super(C, d).__repr__.__get__(None, D) # in Python 2.4+
<bound method D.__repr__ of <instance of D>>

The core developers changed the behavior of ``super`` again, making
my life difficult while I was writing this paper :-/
I cannot make the history of the bugs of ``super`` here, but if you
are using an old version of Python and you find something weird with
``super``, I advice you to have a look at the Python bug tracker
before thinking you are doing something wrong.

.. _Method Resolution Order: http://www.python.org/download/releases/2.3/mro/
.. _new-style classes: http://www.python.org/download/releases/2.2.3/descrintro/
.. _descriptors: http://users.rcn.com/python/download/Descriptor.htm
.. _metaclasses: http://www.ibm.com/developerworks/library/l-pymeta.html
.. _comp.lang.python in May 2003: http://tinyurl.com/5ms8lk
.. _beautiful essay: http://users.rcn.com/python/download/Descriptor.htm
"""

if __name__ == '__main__':
    import doctest; doctest.testmod()
