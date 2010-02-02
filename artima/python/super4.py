"""\
Most languages supporting inheritance support cooperative inheritance too,
i.e.  there is a language-supported way for children methods
to dispatch to their parent method. Cooperation is usually implemented via a
``super`` keyword.  Things are easy when the language support single
inheritance only, since each class has a single parent and there is an
unique concept of super method. Things are difficult when the
language support multiple inheritance: in that case there is no
meaningful concept of super class and of super method, but the programmer
has to understand the intricacies of so-called Method Resolution Order.

Why cooperative hierarchies are tricky
--------------------------------------------

This paper is intended to be very practical, so I will explain
cooperative multiple inheritance with an example. Consider the
following hierarchy (in Python 3):

$$A
$$B
$$C

What is the "superclass" of ``A``? In other words, when I create an
instance of ``A``, which method will be called by
``super().__init__()``?  Notice that I am considering here generic
instances of ``A``, not only direct instances: in particular, an
instance of ``C`` is also an instance of ``A`` and instantiating ``C``
will call ``super().__init__()`` in ``A.__init__`` at some point: the
tricky point is to understand which method will be called
for *indirect* instances of ``A``.

In a single inheritance language there would be an unique answer both
for direct and indirect instances (``object`` is the super class of
``A`` and ``object.__init__`` is the method called by ``super().__init__()``)
but in a multiple inheritance language there is no easy answer. It is
better to say that there is no super class and it is impossible to
know which method will be called by ``super().__init__()`` unless the
entire hierarchy is known in advance. In this case let us assume that
the entire hierarchy is known (i.e. there are no other subclasses
defined in other modules). In particular, this is what happens when we
instantiate ``C``:

>>> c = C()
C.__init__
A.__init__
B.__init__

As you see the super call  in ``C`` dispatches to ``A.__init__`` and the super
call there dispatches to  ``B.__init__`` which in turns dispatches to
``object.__init__``. Therefore *the same super call can dispatch to different
methods*: when ``super().__init__()`` is called directly by instantiating 
``A`` it dispatches to ``object.__init__`` whereas when it is called indirectly
by instantiating ``C`` it dispatches to ``B.__init__``. If somebody 
extends the hierarchy, adds subclasses of ``A`` and instantiated them, 
then the super call in ``A.__init__``
can dispatch to an entirely different method: the super method call 
depends on the instance I am starting from. The precise algorithm 
specifying the order in which the methods are called by ``super`` is
called the Method Resolution Order algorithm, or MRO for short and it 
is discussed in detail in an old essay I wrote years ago.
Interested readers are referred to it.
Here I will take the easy way and I will ask Python.

Given any class, it is possibly to extract its linearization, i.e. the
ordered list of its ancestors plus the class itself: the super call
follow such list to decide which is the right method to dispatch
to. For instance, if you are considering a direct instance of ``A``,
``object`` is the only class the super call can dispatch to:

.. code-block:: python

 >>> A.mro()
 [<class '__main__.A'>, <class 'object'>]

If you are considering a direct instance of ``C``, ``super`` looks at the
linearization of ``C``:

.. code-block:: python

 >>> C.mro()
 [<class '__main__.C'>, <class '__main__.A'>, <class '__main__.B'>, <class 'object'>]

A super call in ``C`` will look first at ``A``, then at ``B`` and finally at
``object``. Finding out the linearization is non-trivial; just to give
an example suppose we add to our hierarchy three classes ``D``, ``E`` and ``F``
in this way:

.. code-block:: python

 >>> class D: pass
 >>> class E(A, D): pass
 >>> class F(E, C): pass
 >>> for c in F.mro():
 ...    print(c.__name__)
 F
 E
 C
 A
 D
 B
 object

As you see, for an instance of ``F`` a super call in ``A.__init__`` 
will dispatch at ``D.__init__`` and not directly at ``B.__init__``! 

The problem with incompatible signatures
----------------------------------------------------

I have just shown that one cannot tell in advance
where the supercall will dispatch, unless one knows the whole hierarchy:
this is quite different from the single inheritance situation and it is
also very much error prone and brittle. 
When you design a hierarchy you will expect for instance that
``A.__init__`` will call ``B.__init__``, but adding classes (and such
classes may be added by a third party) may change the method chain. In this
case ``A.__init__`` (when invoked by an ``F`` instance) will call
``D.__init__``: if the behavior of your code depends on the ordering of the
methods you may get in trouble. Things are worse if one of the methods
in the cooperative chain does not have a compatible signature.

This problem is not theoretical and it happens even in very trivial
hierarchies.  For instance, here is an example of incompatible
signatures in the ``__init__`` method (this affects even Python 2.6,
not only Python 3.X):

.. code-block:: python

 class X(object):
    def __init__(self, a):
        super().__init__()

 class Y(object):
    def __init__(self, a):
        super().__init__()

 class Z(X, Y):
    def __init__(self, a):
        super().__init__(a)

Here instantiating ``X`` and ``Y`` works fine, but as soon as you
introduce ``Z`` you get in trouble since ``super().__init__(a)`` in
``Z.__init__`` will call ``super().__init__()`` in ``X`` which in
turns will call ``Y.__init__`` with no arguments, resulting in a
``TypeError``!  In older Python versions (from 2.2 to 2.5) such
problem can be avoided by leveraging on the fact that
``object.__init__`` accepts any number of arguments (ignoring them) and 
thus replacing ``super().__init__()`` with ``super().__init__(a)``. In Python
2.6+ instead there is no real solution for this problem, except avoiding
``super`` in the constructor or avoiding multiple inheritance.

In general you should use ``super`` only when all the
cooperative methods have consistent signature: that means that you
will not use super in ``__init__`` and ``__new__`` since likely your
constructors will have custom arguments whereas ``object.__init__``
and ``object.__new__`` have no arguments.  However, in practice, you may
inherits from third party classes which do not obey this rule, or
others could derive from your classes without following this rule and
breakage may occur. For instance, I have used ``super`` for years in my
``__init__`` methods and I never had problems because in older Python
versions ``object.__init__`` accepted any number of arguments: but in Python 3
all that code is fragile under multiple inheritance. I am left with 
two choices: removing ``super`` or telling people that
those classes are not intended to be used in multiple inheritance
situations, i.e. the constructors will break if they do that.
Nowadays I tend to favor the second choice. 

Luckily, usually multiple inheritance is used with mixin classes, and mixins do
not have constructors, so that in practice the problem is mitigated.

The intended usage for super
----------------------------------------------------

Even if ``super`` has its shortcomings, there are meaningful use cases for
it, assuming you think multiple inheritance is a legitimate design technique.
For instance, if you use metaclasses and you want to support multiple 
inheritance, you *must* use ``super`` in the ``__new__`` and ``__init__``
methods: there is no problem in doing so, since the constructor for 
metaclasses has a fixed signature *(name, bases, dictionary)*. But metaclasses
are extremely rare, so let me give a more meaningful example for an application
programmer where a design bases on cooperative
multiple inheritances could be reasonable.

Suppose you have a bunch of ``Manager`` classes which
share many common methods and which are intended to manage different resources,
such as databases, FTP sites, etc. To be concrete, suppose there are
two common methods: ``getinfolist`` which returns a list of strings
describing the managed resorce (containing infos such as the URI, the
tables in the database or the files in the site, etc.) and ``close``
which closes the resource (the database connection or the FTP connection).
You can model the hierarchy with a ``Manager`` abstract base class

$$Manager

and two concrete classes ``DbManager`` and ``FtpManager``:

$$DbManager
$$FtpManager

Now suppose you need to manage both a database and an FTP site and suppose that
you think multiple inheritance is a good idea: then you can define a 
``MultiManager`` as follows:

$$MultiManager

Everything works: calling ``MultiManager.close`` will in turn call 
``DbManager.close`` and ``FtpManager.close``. There is no risk of
running in trouble with the signature since the ``close`` and ``getinfolist``
methods have all the same signature (actually they take no arguments at all).
Notice also that I did not use ``super`` in the constructor. 
You see that ``super`` is *essential* in this design: without it, 
only ``DbManager.close`` would be called and your FTP connection would leak. 
The ``getinfolist`` method works similarly: forgetting ``super`` would
mean losing some information. An alternative not using ``super`` would require
defining an explicit method ``close`` in the ``MultiManager``, calling
``DbManager.close`` and ``FtpManager.close`` explicitly, and an explicit
method ``getinfolist`` calling ```DbManager.getinfolist`` and 
``FtpManager.getinfolist``:

$$close
$$getinfolist

This would less elegant but probably clearer and safer so you can always
decide not to use ``super`` if you really hate it. However, if you have
``N`` common methods, there is some boiler plate to write; moreover, every time
you add a ``Manager`` class you must add it to the ``N`` common methods, which
is ugly. Here ``N`` is just 2, so not using ``super`` may work well, 
but in general it is clear that the cooperative approach is more elegant.
Actually, I strongly believe (and always had) that ``super`` and the
MRO are the *right* way to do multiple inheritance: but I also believe
that multiple inheritance itself is *wrong*. For instance, in the
``MultiManager`` example I would not use multiple
inheritance but composition and I would probably use a generalization
such as the following:

$$MyMultiManager

There are languages that do not provide inheritance (even single
inheritance!)  and are perfectly fine, so you should keep an open
mind. There are always many options and the design space is rather
large.  Personally, I always use ``super`` but I use
single-inheritance only, so that my cooperative hierarchies are
trivial.

The magic of super in Python 3
----------------------------------------------------------------------

Deep down, ``super`` in Python 3 is the same as in Python 2.X.
However, on the surface - at the syntactic level, not at the semantic level -
there is a big difference: Python 3 super is smart enough to figure out
*the class it is invoked from and the first argument of the containing
method*. Actually it is so smart that it works also for inner classes
and even if the first argument is not called ``self``.
In Python 2.X ``super`` is dumber and you must tell the class and the
argument explicitly: for instance our first example must be written

.. code-block:: python

 class A(object):
     def __init__(self):
         print('A.__init__')
         super(A, self).__init__()

By the way, this syntax works both in Python 3 *and* in Python 2, this is
why I said that deep down ``super`` is the same. The new feature in
Python 3 is that there is a shortcut notation ``super()`` for 
``super(A, self)``. In Python 3 the (bytecode) compiler is smart enough
to recognize that the supercall is performed inside the class ``A`` so
that it inserts the reference to ``A`` automagically; moreover it inserts
the reference to the first argument of the current method too. Typically
the first argument of the current method is ``self``, but it may be
``cls`` or any identifier: ``super`` will work fine in any case. 

Since ``super()`` knows the class it is invoked from and the class of
the original caller, it can walk the MRO correctly. Such information
is stored in the attributes ``.__thisclass__`` and ``.__self_class__``
and you may understand how it works with the following example:

$$Mother
$$Child

.. code-block:: python

 >>> child = Child()
 <class '__main__.Mother'>
 <class '__main__.Child'>

Here ``.__self__class__`` is just the clas<s of the first argument (``self``)
but this not always the case. The exception is the case of classmethods and
staticmethods taking a class as first argument, such as ``__new__``.
Specifically, ``super(cls, x)`` checks if ``x`` is an instance
of ``cls`` and then sets ``.__self_class__`` to ``x.__class__``; otherwise
(and that happens for classmethods and for ``__new__``) it checks if ``x`` 
is a subclass of ``cls`` and then sets  ``.__self_class__`` to ``x`` directly.
For instance, in the following example

$$C0
$$C1
$$C2

the attribute ``.__self_class__`` is *not* the class of the first argument
(which would be ``type`` the metaclass of all classes) but simply the first
argument:

.. code-block:: python

 >>> C2.c()
 __thisclass__ <class '__main__.C1'>
 __selfclass__ <class '__main__.C2'>
 called classmethod C0.c

So take care that ``__selfclass__`` is not the class of ``self``, if ``self``
is a subclass of ``__thisclass__``.
There is a lot of magic going on, and even more. For instance, this
is a syntax that cannot work:

$$super_external

If you try to run this code you will get a
``SystemError: super(): __class__ cell not found`` and the reason is
obvious: since the ``__init__`` method is external to the class the
compiler cannot infer to which class it will be attached at runtime.
On the other hand, if you are completely explicit and you use the full
syntax, by writing the external method as

$$__init__

everything will work because we are explicitly telling than the method
will be attached to the class ``C``.

There is also a wart of Python 3, pointed out by `Armin Ronacher`_ and
others: the fact that ``super`` should be a keyword but it is
not. Therefore horrors like the following are possible:

$$super_horrors

DON'T DO THAT! Here the called ``__init__`` is the ``__init__`` method
of the object ``None``!!

Also, ``super`` is special and it will not work if
you change its name as in this example:

.. code-block:: python

 # see http://lucumr.pocoo.org/2010/1/7/pros-and-cons-about-python-3
 _super = super
 class Foo(Bar):
     def foo(self):
         _super().foo()

This is unfortunate, since we missed the opportunity to make it a keyword
in Python 3, without good reasons (Python 3 was expected to break compatibility
anyway).

References
---------------------------------------

There is plenty of material about super and multiple inheritance. You
should probably start from the `MRO paper`_, then read `Super
considered harmful`_ by James Knight. A lot of the issues with
``super``, especially in old versions of Python are covered in `Things
to know about super`_. I did spent some time thinking about ways to
avoid multiple inheritance; you may be interested in reading my series
`Mixins considered harmful`_.

.. _MRO paper: http://www.python.org/download/releases/2.3/mro/
.. _new style classes: http://www.python.org/download/releases/2.2.3/descrintro/
.. _Super considered harmful: http://fuhm.net/super-harmful/
.. _Menno Smits: http://freshfoo.com/blog/object__init__takes_no_parameters
.. _Things to know about super: http://www.phyast.pitt.edu/~micheles/python/super.pdf
.. _Mixins considered harmful: http://www.artima.com/weblogs/viewpost.jsp?thread=246341
.. _Armin Ronacher: http://lucumr.pocoo.org/2008/4/30/how-super-in-python3-works-and-why-its-retarded
.. _warts in Python 3: http://lucumr.pocoo.org/2010/1/7/pros-and-cons-about-python-3
"""

import super_external, super_horrors

class A(object):
    def __init__(self):
        print('A.__init__')
        super().__init__()

class B(object):
    def __init__(self):
        print('B.__init__')
        super().__init__()

class C(A, B):
    def __init__(self):
        print('C.__init__')
        super().__init__()

class Manager(object):
    def close(self):
        pass
    def getinfolist(self):
        return []

class DbManager(Manager):
    def __init__(self, dsn):
        self.conn = DBConn(dsn)
    def close(self):
        super().close()
        self.conn.close()
    def getinfolist(self):
        return super().getinfolist() + ['db info']

class FtpManager(Manager):
    def __init__(self, url):
        self.ftp = FtpSite(url)
    def close(self):
        super().close()
        self.ftp.close()
    def getinfolist(self):
        return super().getinfolist() + ['ftp info']


class MultiManager(DbManager, FtpManager):
    def __init__(self, dsn, url):
        DbManager.__init__(dsn)
        FtpManager.__init__(url)

def close(self):
    DbManager.close(self)
    FtpManager.close(self)

def getinfolist(self):
    return DbManager.getinfolist(self) + FtpManager.getinfolist(self)

class MyMultiManager(Manager):
    def __init__(self, *managers):
        self.managers = managers
    def close(self):
        for mngr in self.managers:
            mngr.close()
    def getinfolist(self):
        return sum(mngr.getinfolist() for mngr in self.managers)

class Mother(object):
    def __init__(self):
        sup = super()
        print(sup.__thisclass__)
        print(sup.__self_class__)
        sup.__init__()

class Child(Mother):
    pass

class C0(object):
    @classmethod
    def c(cls):
        print('called classmethod C0.c')

class C1(C0):
    @classmethod
    def c(cls):
        sup = super()
        print('__thisclass__', sup.__thisclass__)
        print('__selfclass__', sup.__self_class__)
        sup.c()

class C2(C1):
    pass

def __init__(self):
    print('calling __init__')
    super(C, self).__init__()

if __name__ == '__main__':
    import doctest; doctest.testmod()
