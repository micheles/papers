r"""Bound and unbound (super) methods
----------------------------------------------------------------

Having established in the first installment of this series
that ``super`` cannot return the
mythical superclass, we may ask ourselves what the hell is returning 
``super`` ;) The truth is that ``super`` returns proxy objects.

Informally speaking, a proxy is an object with
the ability to dispatch to methods of other classes via delegation.
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

 >> print super(C, D).__repr__
 <bound method D.__repr__ of <class '__main__.D'>>

That means that when you call
``super(<class>, <subclass>).method`` in Python 2.2 you do not get
an error::

 >> print super(C, D).__repr__()
 <instance of type>

Here ``D``, seen as an instance of the (meta)class ``type``, is being passed 
to ``__repr__``. This has been fixed in Python 2.3+, where you correctly get
a ``TypeError``:

 >>> print super(C, D).__repr__()
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
the unbound ``super`` object. This is actually untrue. To understand how unbound
methods can be retrieved we need to talk about descriptors.

``super`` and descriptors
----------------------------------------------------

Descriptors (more properly I should speak of the descriptor protocol) were 
introduced in Python 2.2 by Guido van Rossum. Their primary motivation 
is technical, since they were needed to implement the new-style object 
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
signature ``__get__(self, obj, objtyp = None)``. A *descriptor object*
is just an instance of a descriptor class. 

Descriptor objects are intended to be used as attributes (hence their
complete name attribute descriptors). Suppose that ``descr`` is a
given descriptor object used as attribute of a given class C.
Then the syntax ``C.descr`` is actually interpreted by Python as a 
call to ``descr.__get__(None, C)``, whereas the same syntax for an 
instance of C corresponds to a call to ``descr.__get__(c, type(c))``.

Since the combination of descriptors and super is so tricky, the
core developers got it wrong in different versions of Python.
For instance, in Python 2.2 the only way to get method ``__repr__`` is
via the descriptor API::

 >> super(C, d).__repr__.__get__(None, D) # Python 2.2
 <unbound method D.__repr__>

You may check that it works correctly::

 >> print _(d)
 <instance of D>

In Python 2.3 one can get the unbond method by using the ``super(cls, subcls)``
syntax, but the syntax ``super(C, d).__repr__.__get__(None, D)`` also
works; in Python 2.4+ instead the same syntax returns a *bound* method,
not an unbound one:

>>> super(C, d).__repr__.__get__(None, D)
<bound method D.__repr__ of <instance of D>>

This is the correct behavior, so Python 2.3 fixed a major bug of ``super``
but left a minor one there.
I cannot make the history of the bugs of ``super`` here, but if you
are using an old version of Python and you find something weird with
``super``, I advice you to have a look at the Python bug tracker
before thinking you are doing something wrong.

.. _beautiful essay: http://users.rcn.com/python/download/Descriptor.htm

"""

if __name__ == '__main__':
    import doctest; doctest.testmod()
