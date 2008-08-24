r"""
Working with ``super`` is tricky, not only because of the quirks and
bugs of ``super`` itself, but also because you are likely to run into
some gray area of the Python language. For instance, there is a subtle
pitfall which is not directly related to ``super`` but which is often
encountered working with ``super``: the issue of the lookup rules
for special methods, which are somewhat special.

Special attributes are special
----------------------------------------------------------------------------

This came up at least three or four times in the newsgroup, and there 
are various independent bug reports on sourceforge about it, so possibly 
you may face it too. Bjorn Pettersen was the first one who pointed out the
problem to me (see also bug report SF 729913): the issue is that

``super(MyCls, self).__getitem__(5)``

works, but not

``super(MyCls, self)[5]``.

The problem is general to all special methods, not only to ``__getitem__``,
and the explanation for that has to do with the implementation of
attribute lookup for special methods. Clear explanations of what is
going on are provided by Michael Hudson as a comment to the bug report:
SF789262 and by Raymond Hettinger as a comment to the bug report SF805304.
Shortly put, this is not a problem of ``super`` per se, the problem is
that the special call ``x[5]`` (using ``__getitem__`` as example) is
converted to  ``type(x).__getitem__(x,5)``
*only if* ``__getitem__`` is explicitely defined in ``type(x)``. If 
``type(x)`` does not define ``__getitem__`` directly, but only 
indirectly via delegation (i.e. overriding ``__getattribute__``),
then the second form works but not the first one.

This restriction will likely stay in Python, so it has to be
considered just a documentation bug, since nowhere in
the docs it is mentioned that special calling syntaxes (such as
the ``[]`` call, the ``iter`` call, the ``repr`` call, etc. etc.)
are special and bypass ``__getattribute__``. The advice is:
just use the more explicit form and everything will work.

``super`` does not work with meta-attributes
----------------------------------------------------------------------

Even when ``super`` is right, its behavior may be surprising, unless
you are deeply familiar with the intricacies of the Python object
model. For instance, ``super`` does not 
play well with the ``__name__`` attribute of classes, even if it
works well for the ``__doc__`` attribute and other regular
class attributes. Consider this example:

 >>> class B(object):
 ...     "This is class B"
 ... 
 >>> class C(B):
 ...     pass
 ... 

The special (class) attribute ``__doc__`` is retrieved as you would expect:

 >>> super(C, C).__doc__ == B.__doc__
 True

On the other hand, the special attribute ``__name__`` is not 
retrieved correctly:

 >>> super(C, C).__name__ # one would expect it to be 'B'
 Traceback (most recent call last):
   File "<stdin>", line 1, in ?
 AttributeError: 'super' object has no attribute '__name__'

The problem is that ``__name__`` is not just a plain class
attribute: it is actually a *getset descriptor* defined on
the metaclass ``type`` (try to run  ``help(type.__dict__['__name__'])``
and you will see it for yourself). More in general, ``super`` has
problems with meta-attributes, i.e. class attributes of metaclasses.

Meta-attributes differs from regular attributes since they are not 
transmitted to the instances of the instances. Consider this example::

 class M(type):
     "A metaclass with a class attribute 'a'."
     a = 1 

 class B:
     "An instance of M with a meta-attribute 'a'."
     __metaclass__ = M

 class C(B):
     "An instance of M with the same meta-attribute 'a'"

 if __name__ == "__main__":
     print B.a, C.a # => 1 1 
     print super(C,C).a #=> attribute error

If you run this, you will get an attribute error. This is a case
where ``super`` is doing the *right* thing, since 'a' is *not* inherited 
from B, but it comes directly from the metaclass, so 'a'
is *not* in the MRO of C. A similar thing happens for the ``__name__``
attribute (the fact that it is a descriptor and not a plain 
attribute does not matter), so ``super`` is working correctly, but
still it may seems surprising at first.
You can find the
rationale for this behaviour in `my second article with David Mertz`_;
in the case of ``__name__`` it is obvious though: you don't want
all of your objects to have a name, even if all your classes do.

When it comes to ``super`` don't trust even Guido himself!
---------------------------------------------------------------------------

``super`` is so tricky that even Guido got is wrong. For instance,
in order to explain how ``super`` works, Guido describes a
"fully functional implementation of the super() built-in class in 
pure Python" in "Unifying types and classes in Python 2.2", which
I report here for your convenience:

$$Super

Unfortunately, this implementation is more harmful than helpful, since
the current ``super`` DOES NOT work in the same way :-(
Take for instance this example:

 >>> class C(object):
 ...    f = 'C.f'
 >>> class D(C):
 ...    f = 'D.f'

Here the ``super`` works fine, 

 >>> print super(D, D()).f 
 C.f

but the class ``Super`` described by Guido will raise an attribute
error when invoked as ``Super(D, D()).f``.  Therefore ``Super`` is NOT 
equivalent to the currently implemented ``super`` built-in.

Miscellaneous ``super`` bugs in earlier versions of Python
-----------------------------------------------------------------

There are various ``super`` pitfalls currently undocumented of which
the experienced Python programmer should be aware of.

The unbound form of ``super`` does not play well with pydoc. 
The problems is still there in Python 2.3.4 (see bug report SF729103)

 >>> class B(object): pass
 ... 
 >>> class C(B):
 ...     s=super(B)
 ... 
 >>> help(C)
 Traceback (most recent call last):
   ...
   ... lots of stuff here
   ...
 File "/usr/lib/python2.3/pydoc.py", line 1198, in docother
    chop = maxlen - len(line)
 TypeError: unsupported operand type(s) for -: 'type' and 'int'

I have not yet clear what the cause is, but it is certainly quite
tricky. An incompatibility between  ``super`` was reported by Christian Tanzer
(SF902628); if you run the following, you will get a ``TypeError``::

  class C(object):
      pass

  C.s = super(C) 

  if __name__ == "__main__":
      import doctest, __main__
      doctest.testmod(__main__)

BTW, I don't think this is related to ``super`` only since I have
found similar problems when playing with descriptors and doctest
some time ago (but I cannot reproduce the bug right now).

Finally, there may be other bugs and pitfalls I am not aware of. Certainly
there are many other issues and bugs in previous versions of Python that
I have not mentioned here, since they have been fixed, but that you may
encounter if you use earlier versions of Python.

.. _my second article with David Mertz: http://www-128.ibm.com/developerworks/linux/library/l-pymeta2 
"""

class Super(object):
    "Guido's implementation of super in http://www.python.org/download/releases/2.2.3/descrintro"
    def __init__(self, type, obj=None):
        self.__type__ = type
        self.__obj__ = obj
    def __get__(self, obj, type=None):
        if self.__obj__ is None and obj is not None:
            return Super(self.__type__, obj)
        else:
            return self
    def __getattr__(self, attr):
        if isinstance(self.__obj__, self.__type__):
            starttype = self.__obj__.__class__
        else:
            starttype = self.__obj__
        mro = iter(starttype.__mro__)
        for cls in mro:
            if cls is self.__type__:
                break
        # Note: mro is an iterator, so the second loop
        # picks up where the first one left off!
        for cls in mro:
            if attr in cls.__dict__:
                x = cls.__dict__[attr]
                if hasattr(x, "__get__"):
                    x = x.__get__(self.__obj__)
                return x
        raise AttributeError, attr


if __name__ == '__main__':
    import doctest; doctest.testmod()
