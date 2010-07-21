r"""
Working with ``super`` is tricky, not only because of the quirks and
bugs of ``super`` itself, but also because you are likely to run into
some gray area of the Python language itself. In particular, in order to
understand how ``super`` works, you need to understand really well
how attribute lookup works, including the tricky cases of
special attributes and metaclass attributes.
Moreover, even if you know perfectly well how ``super`` works,
interacting with a third party library using (or not using) ``super``
is still non-obvious. At the end, I am led to believe that the problem is not
``super``, but the whole concept of multiple inheritance and cooperative
methods in Python.

Special attributes are special
----------------------------------------------------------------------------

This issue came up at least three or four times in the Python
newsgroup, and there 
are various independent bug reports on sourceforge about it,
you may face it too. Bjorn Pettersen was the first one who pointed out the
problem to me (see also bug report 729913_): the issue is that

``super(MyCls, self).__getitem__(5)``

works, but not

``super(MyCls, self)[5]``.

The problem is general to all special methods, not only to ``__getitem__``
and it is a consequence of the implementation of
attribute lookup for special methods. Clear explanations of what is
going on are provided by Michael Hudson as a comment to the bug report:
789262_ and by Raymond Hettinger as a comment to the bug report 805304_.
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

.. _783528: http://bugs.python.org/issue783528
.. _729913: http://bugs.python.org/issue729913
.. _789262: http://bugs.python.org/issue789262
.. _805304: http://bugs.python.org/issue805304

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


There are certainly other bugs and pitfalls which I have not mentioned here
because I think are not worth mention, or because I have forgot them, or
also because I am not aware of them all. So, be careful when you use
``super``, especially in earlier versions of Python.

.. _my second article with David Mertz: http://www-128.ibm.com/developerworks/linux/library/l-pymeta2 

Remember to use super consistently
--------------------------------------------------------------------

Some years ago James Knight wrote an essay titled
`Super considered harmful`_ where he points out a few shortcomings
of ``super`` and he makes an important recommendation: 
*use super consistently, and document that you use it, as it is part of the 
external interface for your class, like it or not*.
The issue is that a developer inheriting from a hierarchy written by somebody
else has to know if the hierarchy uses ``super`` internally
or not. For instance, consider this case, where the library author has
used ``super`` internally:

$$library_using_super

If the application programmer knows that the library uses ``super`` internally,
she will use ``super`` and everything will work just fine; but it she does not
know if the library uses ``super`` she may be tempted to call ``A.__init__``
and ``B.__init__`` directly, but this will end up in having ``B.__init__``
called twice! 

 >>> from library_using_super import A, B

 >>> class C(A, B):
 ...     def __init__(self):
 ...         print "C",
 ...         A.__init__(self)
 ...         B.__init__(self)
  
 >>> c = C() 
 C A B B

On the other hand, if the library does *not* uses ``super`` internally,

$$library_not_using_super

the application programmer cannot use ``super`` either, otherwise
``B.__init__`` will not be called:

 >>> from library_not_using_super import A, B

 >>> class C(A,B):
 ...     def __init__(self):
 ...         print "C",
 ...         super(C, self).__init__()

 >>> c = C()
 C A

So, if you use classes coming from a library in a multiple inheritance
situation, you must know if the classes were intended to be
cooperative (using ``super``) or not. Library author should always
document their usage of ``super``.

Argument passing in cooperative methods can fool you
----------------------------------------------------------------------

James Knight devolves a paragraph to the discussion of argument passing
in cooperative methods. Basically, if you want to be safe, all your cooperative
methods should have a compatible signature. There are various ways of
getting a compatible signature, for instance you could accept everything (i.e.
your cooperative methods could have signature ``*args, **kw``) which is
a bit too much for me, or all of your methods could have exactly the same
arguments. The issue comes when you have default arguments, since your
MRO can change if you change your hierarchy, and argument passing may
break down. Here is an example:

$$cooperation_ex

>>> from cooperation_ex import D
>>> d = D()
D
B with a=None
C with a=None
A

This works, but it is fragile (you see what will happen if you change
``D(B, C)`` with ``D(C, B)``?) and in general it is always difficult
to figure out which arguments will be passed to each method and in
which order so it is
best just to use the same arguments everywhere (or not to use
cooperative methods altogether, if you have no need for cooperation).
There is no shortage of examples of trickiness in multiple inheritance
hierarchies; for instance I remember a post from comp.lang.python
about the `fragility of super`_ when changing the base class.

Also, beware of situations in which you have
some old style classes mixing with new style classes: the result may
depend on the order of the base classes (see examples 2-2b and 2-3b
in `Super considered harmful`_).


UPDATE: the introduction of Python 2.6 made the
special methods ``__new__`` and ``__init__`` even more brittle with respect to
cooperative super calls.

Starting from Python 2.6 the special methods ``__new__`` and
``__init__`` of ``object`` do not take any argument, whereas
previously the had a generic signature, but all the arguments were
ignored. That means that it is very easy to get in trouble if your
constructors take arguments. Here is an example:

$$A
$$B
$$C

As you see, this cannot work: when ``self`` is an instance of ``C``,
``super(A, self).__init__()`` will call ``B.__init__`` without
arguments, resulting in a ``TypeError``. In older Python you could
avoid that by passing ``a`` to the super calls, since
``object.__init__`` could be called with any number of arguments.
This problem was recently pointed out by `Menno Smits`_ in his blog
and there is no way to solve it in Python 2.6, unless you change all
of your classes to inherit from a custom ``Object`` class with an
``__init__`` accepting all kind of arguments, i.e. basically reverting
back to the Python 2.5 situation.

Conclusion: is there life beyond super?
-------------------------------------------------------

In this series I have argued that ``super`` is tricky; I think
nobody can dispute that. However the existence of dark corners is not
a compelling argument against a language construct: after all, they
are rare and there is an easy solution to their obscurity,
i.e. documenting them. This is what I have being doing all along.
On the other hand, one may wonder if all ``super`` warts aren't
hints of some serious problem underlying. It may well
be that the problem is not with ``super``, nor with cooperative
methods: the problem may be with multiple inheritance itself.

I personally liked super, cooperative methods and multiple inheritance
for a couple of years, then I started working with Zope and my mind
changed completely. Zope 2 did not use super at all but is a mess anyway,
so the problem is multiple inheritance itself. Inheritance
makes your code heavily coupled and difficult to follow (*spaghetti
inheritance*).  I have not found a real life problem yet that I could
not solve with single inheritance + composition/delegation in a better
and more maintainable way than using multiple inheritance.
Nowadays I am *very* careful when using multiple inheritance.

People should be educated about the issues; moreover people should
be aware that there are alternative to multiple inheritance in
other languages. For instance Ruby uses 
mixins (they are a restricted multiple inheritance without cooperative
methods and with a well defined superclass, but they do not solve
the issue of name conflicts and the issue with the ordering of
the mixin classes); recently some people proposed the concepts
of traits_ (restricted mixin where name conflicts must be solved explicitely
and the ordering of the mixins does not matter) which is interesting.

In CLOS multiple inheritance works better since (multi-)methods
are defined outside classes and ``call-next-method`` is well integrated
in the language; it is simpler to track down the ancestors
of a single method than to wonder about the full class hierarchy.
The language SML (which nobody except academics use, but would deserve
better recognition) goes boldly in the direction of favoring composition over
inheritance and uses functors to this aim.

Recently I have written a trilogy of papers for Stacktrace discussing
why multiple inheritance and mixins are a bad idea and suggesting
alternatives. I plan to translate the series and to publish here in
the future. For the moment you can use the Google Translator. The
series starts from here_ and it is a recommended reading if you ever
had troubles with mixins.

.. _here: http://stacktrace.it/articoli/2008/06/i-pericoli-della-programmazione-con-i-mixin1/
.. _Super considered harmful: http://fuhm.net/super-harmful/
.. _fragility of super: http://tinyurl.com/3jqhx7
.. _traits: http://www.iam.unibe.ch/~scg/Research/Traits/
.. _Menno Smits: http://freshfoo.com/blog/object__init__takes_no_parameters
"""

import library_using_super, library_not_using_super, cooperation_ex

class A(object):
    def __init__(self, a):
        super(A, self).__init__() # object.__init__ cannot take arguments

class B(object):
    def __init__(self, a):
        super(B, self).__init__() # object.__init__ cannot take arguments

class C(A, B):
    def __init__(self, a):
        super(C, self).__init__(a) # A.__init__ takes one argument 

if __name__ == '__main__':
    import __main__, doctest; doctest.testmod(__main__)
