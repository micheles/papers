r"""\
When working with ``super``, virtually everybody uses the two-argument syntax
``super(type, object-or-type)`` which returns a
*bound super object* (bound to the second argument, an instance
or a subclass of the first argument). 
However, ``super`` also supports a single-argument syntax
``super(type)`` - fortunately very little used - 
which returns an *unbound super object*. 
Here I argue that unbounds super objects are a wart of the language
and should be removed or deprecated (and Guido agrees).

The secrets of unbound super objects
----------------------------------------------------------

Let me begin by clarifying a misconception
about bound super objects and unbound super objects. 
From the names, you may think that if ``super(C, c).meth``
returns a bound method then ``super(C).meth`` returns
an unbound method: however, this is a *wrong* expectation.
Consider for instance the following example:

>>> class B1(object): 
...     def f(self): 
...         return 1
...     def __repr__(self): 
...         return '<instance of %s>' % self.__class__.__name__
...
>>> class C1(B1): pass
...

The unbound super object ``super(C1)`` does not dispatch
to the method of the superclass:

>>> super(C1).f
Traceback (most recent call last):
  ...
AttributeError: 'super' object has no attribute 'f'

i.e. ``super(C1)`` is not a shortcut for the bound super object
``super(C1, C1)`` which dispatches properly:

>>> super(C1, C1).f
<unbound method C1.f>

Things are more tricky if you consider methods defined in ``super``
(remember that ``super`` is class which defines a few methods, such as
``__new__``, ``__init__``, ``__repr__``, ``__getattribute__`` and 
``__get__``) or special attributes inherited from ``object``. In our example
``super(C1).__repr__`` does not give an error,

>>> print super(C1).__repr__() # same as repr(super(C1))
<super: <class 'C1'>, NULL>

but it is not dispatching to the ``__repr__`` method in the base class
``B1``: instead, it is retrieving the ``__repr__`` method defined in 
``super``, i.e.  it is giving something completely different.

Very tricky. You *cannot* use unbound ``super`` object 
to dispatch to the the upper methods in the hierarchy.
If you want to do that, you must use the two-argument syntax
``super(cls, cls)``, at
least in recent versions of Python. We said before
that Python 2.2 is buggy in this respect, i.e. ``super(cls, cls)``
returns a *bound* method instead of an *unbound* method::

 >> print super(C1, C1).__repr__ # buggy behavior in Python 2.2
 <bound method C1.__repr__ of <class '__main__.C1'>>

Unbound super objects must be turned into bound objects in order to
make them to dispatch properly. That can be done via the descriptor
protocol. For instance, I can convert ``super(C1)`` in a super object
bound to ``c1`` in this way:

>>> c1 = C1()
>>> boundsuper = super(C1).__get__(c1, C1) # this is the same as super(C1, c1)

Now I can access the bound method ``c1.f`` in this way:

 >>> print boundsuper.f
 <bound method C1.f of <instance of C1>>

The unbound syntax is a mess
------------------------------------------------------------------

Having established that the unbound syntax does not return unbound methods 
one might ask what its purpose is.
The answer is that ``super(C)`` is intended to be used as an attribute in 
other classes. Then the descriptor magic will automatically convert the 
unbound syntax in the bound syntax. For instance:

 >>> class B(object):
 ...     a = 1
 >>> class C(B):
 ...     pass
 >>> class D(C):
 ...     sup = super(C)
 >>> d = D()
 >>> d.sup.a
 1

This works since ``d.sup.a`` calls ``super(C).__get__(d,D).a`` which is
turned into ``super(C, d).a`` and retrieves ``B.a``.

There is a single use case for the single argument 
syntax of ``super`` that I am aware of, but I think it gives more troubles 
than advantages. The use case is the implementation of *autosuper* made 
by Guido on his essay about `new-style classes`_.

.. _new-style classes: http://www.python.org/download/releases/2.2.3/descrintro/#cooperation

The idea there is to use the unbound super objects as private
attributes. For instance, in our example, we could define the
private attribute ``__sup`` in the class ``C`` as the unbound
super object ``super(C)``:

 >>> C._C__sup = super(C)

With this definition inside the methods the syntax 
``self.__sup.meth`` can be used
as an alternative to ``super(C, self).meth``. The advantage is 
that you avoid to repeat the name of the class in the calling
syntax, since that name is hidden in the mangling mechanism of
private names. The creation of the ``__sup`` attributes can be hidden 
in a metaclass and made automatic. So, all this seems to work: but
actually this *not* the case.

Things may wrong in various cases, for instance for classmethods,
as in this example:

$$test__super

The test will print a message ``'super' object has no attribute 
'meth'.`` The issue here is that  ``self.__sup.meth`` works
but ``cls.__sup.meth`` does not, unless the ``__sup`` descriptor
is defined at the metaclass level.

So, using a ``__super`` unbound super object is not a robust solution
(notice that everything would work by substituting  ``self.__super.meth()``
with ``super(C,self).meth()`` instead). 
In Python 3.0 all this has been resolved in a much better way.

.. There are other ways to avoid repeating the class name, see for instance my cookbook recipe [#]_.

If it was me, I would just remove the single argument syntax of
``super``, making it illegal. But this would probably break someone
code, so I don't think it will ever happen in Python 2.X. 
I did ask on the Python 3000 mailing list about removing unbound
super objects (the title of the thread was
*let's get rid of unbound super*) and this was Guido's
reply:

 *Thanks for proposing this -- I've been scratching my head wondering
 what the use of unbound super() would be. :-) I'm fine with killing it
 -- perhaps someone can do a bit of research to try and find out if
 there are any real-life uses (apart from various auto-super clones)?*
 --- Guido van Rossum

Unfortunaly as of now unbound super objects are still around in Python
3.0, but you should consider them morally deprecated.

Bugs of unbound super objects in earlier versions of Python
-----------------------------------------------------------------

The unbound form of ``super`` is pretty buggy in Python 2.2 and Python 2.3.
For instance, it does not play well with pydoc.
Here is what happens with Python 2.3.4 (see also bug report 729103_):

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

In Python 2.2 you get an AttributeError instead, but still ``help``
does not work.

Moreover, an incompatibility between the unbound form of ``super`` and doctest
in Python 2.2 and Python 2.3 was reported by Christian Tanzer (902628_).
If you run the following

::

 class C(object):
     pass

  C.s = super(C)

 if __name__ == '__main__':
     import doctest, __main__; doctest.testmod(__main__)

you will get a 

::

 TypeError: Tester.run__test__: values in dict must be strings, functions or classes; <super: <class 'C'>, NULL>

Both issues are not directly related to ``super``: they are bugs
with the ``inspect`` and ``doctest`` modules not recognizing descriptors
properly. Nevertheless, as usual,  they
are exposed by ``super`` which acts as a magnet for subtle bugs.
Of course, there may be other bugs I am not aware of; if you know of other
issues, just add a comment here.

.. _729103: http://bugs.python.org/issue729103
.. _902628: http://bugs.python.org/issue902628

Appendix
-------------------------------------------

In this appendix I give some test code for people wanting to understand
the current implementation of ``super``. Starting from Python 2.3+,
``super`` defines the following attributes::

 >> vars(super).keys() 
 ['__thisclass__',
 '__new__',
 '__self_class__',
 '__self__',
 '__getattribute__',
 '__repr__',
 '__doc__',
 '__init__',
 '__get__']

In particular super objects
have attributes ``__thisclass__`` (the first argument passed to
``super``) ``__self__`` (the second argument passed to ``super`` or
``None``) and ``__self_class__`` (the class of ``__self__``, ``__self__`` 
or ``None``). You may check that the following assertions hold true:

$$test_super

The tricky point is the ``__self_class__`` attribute, which is the class
of ``__self__`` only if ``__self__`` is an instance of ``__thisclass__``,
otherwise  ``__self_class__`` coincides with ``__self__``. Python 2.2
was buggy because it failed to make that distinction, so it could not
distinguish bound and unbound methods correctly.
"""

def test__super():
  "These tests work for Python 2.2+"

  class B(object):
      def __repr__(self):
          return '<instance of %s>' % self.__class__.__name__
      def meth(cls):
          print "B.meth(%s)" % cls
      meth = classmethod(meth) # I want this example to work in older Python

  class C(B):
      def meth(cls):
          print "C.meth(%s)" % cls
          cls.__super.meth()
      meth = classmethod(meth)

  C._C__super = super(C)

  class D(C):
      pass

  D._D__super = super(D)

  d = D()

  try:
      d.meth()
  except AttributeError, e:
      print e
  else:
      raise RuntimeError('I was expecting an AttributeError!')

def test_super():
    "These tests work for Python 2.3+"

    class B(object):
         pass

    class C(B):
        pass

    class D(C):
       pass

    d = D()

    # instance-bound syntax
    bsup = super(C, d)
    assert bsup.__thisclass__ is C
    assert bsup.__self__ is d
    assert bsup.__self_class__ is D

    # class-bound syntax
    Bsup = super(C, D)
    assert Bsup.__thisclass__ is C
    assert Bsup.__self__ is D
    assert Bsup.__self_class__ is D

    # unbound syntax
    usup = super(C)    
    assert usup.__thisclass__ is C
    assert usup.__self__ is None
    assert usup.__self_class__ is None

if __name__ == '__main__':
    test__super()
    test_super()
    import doctest; doctest.testmod()
