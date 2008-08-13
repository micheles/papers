r"""An implementation of traits in Python
==================================================================

:Author: Michele Simionato
:Date: XXX
:Version: XXX
:Licence: BSD

Motivation
------------------------------------------------

Multiple inheritance is a hotly debated topic. 
The supporters of multiple inheritance 
claim that it makes code shorter and easier
to read, whereas the opposers claim that is makes
code more coupled and more difficult to understand.  I have
spent some time in the past facing the intricacies of `multiple
inheritance in Python`_ ; moreover I have worked with frameworks making
large use of multiple inheritance (I mean Zope 2) and nowadays I am in
the number of the people who oppose multiple inheritance.  Therefore I
am interested in alternatives. In recent years, the approach
of traits_ has gained some traction in a few languages, so I have
decided to write a library to implement traits in Python, for
experimentation purposes. The library is meant for framework builders,
people who are thinking about writing a framework based on multiple
inheritance (typically via the common mixin approach) but
are not convinced that this is the best solution and would like to try
an alternative. This library is also for the authors of existing frameworks
based on mixins that are unsatisfied and would like to convert their
framework to traits.

Are traits a better solution than multiple inheritance and mixins?
In theory I think so, otherwise I would not have write this library, but
in practice (as always) things may be different. It may well be that in
practice using traits or using mixins does not make a big difference
and that changing paradigm is not worth the effort; or the opposite may be
true. The only way to know is to try, to build software based on traits
and to see how it scale *in the large* (in the small, more or less any
approach works fine; it is only by programming in the large that you
can see the differences).
This is the reason why I am releasing
this library with a liberal licence, so that people can try it out and
see how it works. The library is meant to play well with pre-existing
frameworks. As an example, I will show here how you could rewrite
Tkinter to use traits instead of mixins. Of course, I am not advocating
rewriting Tkinter: it would be silly at this point of its history and
pointless; but it may have sense (or not) to rewrite
your own framework using traits, perhaps a framework which is used
in house but has not been released yet.

Finally, let me notice that I am not the only one to have
implemented traits for Python; after finishing my implementation
I made a research and discovered a few implementations. The
biggest effort seems to be `Enthought Traits`_ which I did
not try, since the tutorial required Microsoft Windows,
wxPython and enthought.traits.ui.wx to run. My implementation
does not require anything, is short and I am committed
to keep it short even in the future, according to
the principle of `less is more`_.
 
.. _less is more: http://www.artima.com/weblogs/viewpost.jsp?thread=236286
.. https://svn.enthought.com/enthought/wiki/Traits


.. _multiple inheritance in Python: MRO

What are traits?
------------------------------------------------------------

The word *traits* has many meanings; I will refer to it in the sense of
the paper `Traits - Composable Units of Behavior`_ which 
implements them in Squeak/Smalltalk. The paper appeared in 2003, but most
of the ideas underlying traits have been floating around for at least
30 years. There is also a trait implementation for `PLT Scheme`_ which is
close in spirit (if not in practice) to what I am advocating here.
The library
you are reading about is by no means intended as a porting of the Smalltalk
library: I am just stealing some of the ideas from that paper to implement
a Pythonic alternative to mixins which, for lack of a better name, I have
decided to call traits, but I feel no obligation whatsoever to be consistent
with the Smalltalk library. In doing so, I am following a long tradition,
since a lot of languages use the name "traits" to mean something completely
different from the Smalltalk meaning. For instance the languages
Fortress and Scala use the name "trait" but they mean by it what is
usually called a "mixin". 
For me a trait is a bunch of methods and attributes will the following
properties:

1. the methods/attributes go logically together;
2. traits can be used to enhance independent classes;
3. the ordering is not important, so enhancing a class first with trait T1 and 
   then with trait T2 or viceversa is the same;
4. if traits T1 and T2 have names in common, enhancing a class both
   with T1 and T2 raises an error unless unless you specify *explicitely*
   how the overriding of the common names has to be made;
5. a class can be seen both as a composition of traits and as an homogeneous
   entity.

Properties 3 ,4, 5 are the distinguishing properties of traits with respect
to multiple inheritance and mixins.
In particular, because of 3 and 4, all the complications with the Method
Resolution Order disappear and the overriding is never implicit.
Property 5 has to be intended in the sense that a trait implementation
must provide introspection facilities to make seemless the transition
between classes viewed as atomic entities and as composed entities.

A hands-on example
------------------------------------------------------

For pedagogical purpose I will show here how you could rewrite a
Tkinter class to use traits instead of mixins. Consider the
``Tkinter.Widget`` class, which is derived by the base class
``BaseWidget`` and the mixin classes
``Tkinter.Grid``, ``Tkinter.Pack`` and ``Tkinter.Place``: we want to
rewrite it to use traits instead of mixins. The ``mtrait`` module
provides a class decorator named ``include`` to do exactly that; it is
enough to replace the multiple inheritance syntax

.. code-block:: python

  class Widget(BaseWidget, Grid, Pack, Place):
       pass

with a class decorator syntax:

.. code-block:: python

 @include(Pack, Place, Grid) # this syntax requires Python 2.6+
 class Widget(BaseWidget):
     pass

For compatibility with old versions of Python (before Python 2.6)
the ``include`` class decorator provides some magic to work even
inside classes:

.. code-block:: python

 class Widget(BaseWidget): # this syntax works in Python 2.2+
     include(Pack, Place, Grid)

The preferred way however is to use the ``@`` notation, if available.
I said that the conversion from mixins to traits was easy: but actually
I lied since if you try to execute the code I just wrote you will
get an ``OverridingError``:

>>> from Tkinter import *
>>> class Widget(BaseWidget):
...     include(Pack, Place, Grid)
Traceback (most recent call last):
  ...
OverridingError: Pack.{info, config, configure, slaves, forget} overriding names in Place

The reason for the error is clear: both ``Pack`` and ``Place`` provide
methods called ``{info, config, configure, slaves, forget}`` 
and the traits implementation cannot figure out
which ones to use.  This is a feature, since it forces you to be
explicit. In this case, if we want to be consistent with
multiple inheritance rules, clearly we want the methods coming from
the first class (i.e. ``Pack``) to have precedence and we must say so:

$$TOSWidget

Notice that we had to specify the ``propagate`` method too, since
it is common between ``Pack`` and ``Grid``.

You can check that the TOSWidget class works, for instance by defining a
label widget as follows (remember that ``TOSWidget`` inherits its signature
from  ``BaseWidget``):

>>> label = TOSWidget(master=None, widgetName='label', cnf=dict(text="hello"))

You may visualize the widget by calling the ``.pack`` method:

>>> label.pack()

This should open a small window with the message "hello" inside it.
As you see, in a lot of cases replacing mixins with traits is fairly
straightforward: however, in some cases you can run into problems. In
particular if you have a cooperative multiple inheritance hierarchy
(i.e. you use ``super`` in the mixin classes) you will have to rewrite
your code not to use ``super`` (there are no problem if you use
``super`` ony in methods of the base class). This is on purpose: 
traits are not a replacement for
cooperative multiple inheritance, you should uses traits if you think
that cooperative multiple inheritance is a bad idea and you are
willing to remove it from your code. Old frameworks written before the
introduction of ``super`` (such as Tkinter and Zope 2) are easier to
convert in this respect, since they are non-cooperative. 
The ``mtrait`` module is
intended for framework writers, so it assumes you can change the source
code of your framework if you want; of course, it aims to
reduce the needed changes as much as possible. 

How does it work
---------------------------------------------------------

The implementation of traits provided is an ``mtrait`` is short and
relatively simple but if makes use of sophisticated techniques.  The
class decorator ``include`` takes a set of mixin classes in input,
determines the right metaclass to use and returns a class in output;
the metaclass enhances the class in output by adding to it a special
attribute ``__traits__``, a custom ``__getattr__`` method and a custom
``__getstate__`` method. The
``__traits__`` attribute is a ``TraitContainer`` instance akin to a
dictionary of trait objects (one for each mixin class); the
``__getattr__`` method allows automatic dispatching to them;
the ``__getstate__`` method is needed to preserve pickleability of
the instances (assuming the original class before decoration was
pickleable, of course)>

Now let me describe a trait object. The ``mtrait`` module provides a
``Trait`` class which you can use directly, instead of relying on the
magic of ``include``. You can see a trait as a wrapper around an
inner object.  Usually you want to convert a mixin class into a trait; you
can do it as follows:

  ``trait = Trait(Mixin, Mixin.__name__)``

It is also common to convert a module into a trait:

  ``trait = Trait(module, module.__name__)``

Trait objects are attribute descriptors, i.e. they can used as class
attributes; when a trait is called from a class or an instance, we
say that it is bound to the class or the instance.

Here is an example of usage:

>>> pack = Trait(Pack, 'Pack') # define an unbound trait
>>> pack
<Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} >

>>> class MyWidget(BaseWidget): # add the trait to a class
...      p = pack

Here is an example of trait a bound to a class:

>>> MyWidget.p #doctest: +ELLIPSIS
<Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} bound to <class __main__.MyWidget at 0x...>>

Here is an example of a trait bound to an instance:

>>> lbl = MyWidget(None, 'label', dict(text='hello'))
>>> lbl.p #doctest: +ELLIPSIS
<Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} bound to <__main__.MyWidget instance at 0x...>>

You can call the methods of a trait as follows:

>>> lbl.p.pack()
>>> lbl.p.info().keys()
['side', 'ipady', 'ipadx', 'in', 'pady', 'padx', 'anchor', 'expand', 'fill']

The descriptor magic works in such a way that the instance methods get
as first argument the instance to which the trait is bound; on the other
hand, for traits bound to classes, you get an unbound method associated
to the given class:

>>> MyWidget.p.pack
<unbound method MyWidget.pack_configure>

Different traits or mixins can be composed into a ``TraitContainer``:
for instance, you could define

>>> tc = TraitContainer.from_([Pack, Place, Grid])
>>> tc
<Traits Grid, Place, Pack >

The ``TraitContainer`` object has attributes ``Pack``, ``Place`` and ``Grid``
which are traits corresponding to the mixin classes; for instance

>>> tc.Pack
<Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} >

The magic of ``include``
----------------------------------------------------------------------

Even if you could build your trait objects yourself as explained in
the previous paragraph, and you could implement the dispatch to traits
by hand, usually it is easier if you just rely on the magic of
the ``include`` class decorator, which is doing a lot of work on your
behalf. In particular, internally the class decorator
builds a trait object for each mixin and store all of them into the
``__traits__`` attribute;
moreover it adds a suitable ``__getattr__``
method to the decorated class. 
Finally the class decorator adds a suitable ``__getstate_``
method so that your objects stay pickleable if they were originally
pickleable (adding ``__getattr__`` without adding ``__getstate__`` would
break pickle). Notice that the original class should not define
``__getattr__`` or ``__getstate__`` of its own, otherwise an 
``OverridingError`` is raised. This is intended to prevent
accidental overriding; if you really know what you are doing,
you can always replace the default implementation with your own
after class creation.
If you want to understand the details, you are welcome
to give a look at the implementation, which is pretty small.

The goal of the ``mtrait`` module it to modify the standard
Python object model, turning it into a Trait Object System (TOS for short): 
TOS classes (i.e. instances of ``MetaTOS``) behave differently from regular
classes. In particular TOS classes must not support multiple inheritance
and must forbid your from defining your own ``__getattr__`` and 
``__getstate__`` methods; these
two fundamental properties must be preserved under inheritance (i.e. the
son of a TOS class must be a TOS class) and therefore the implementation
requires necessarily metaclasses.

People who do not like magic may do everything explicitely, by using
the ``__metaclass__`` hook and by specifying the mixins with ``__mixins__``:

$$TOSWidget2

``include`` does more than that, since it
takes care automatically of possible metaclass conflicts. In the case of
Tkinter there is no such problem, since ``BaseWidget`` is just a traditional
old-style class and the metaclass for ``TOSWidget2`` is just ``TOSMeta``:

>>> type(TOSWidget)
<class 'mtrait.TOSMeta'>

However, in general you may need to build your Trait Based Framework
on top of pre-existing classes with a nontrivial metaclass, for
instance Zope classes; in that case having a class decorator smart
enough to figure out the right metaclass to use is a convenient. Here
is an example using Plone classes:

>> from OSF.Folder import Folder


when you call
``super(<class>, <subclass>).method``
>> from zope.plone import BaseContent
>> class 

>> type(X)
ExtensionClassMetaTOS


<type 'ExtensionClass.ExtensionClass'>

Why multiple inheritance is forbidden
----------------------------------------------------------

As I said, the mother metaclass of the Trait Object System ``MetaTOS`` forbids
multiple inheritance and if you try to multiple inherit from a TOS
class and another class you will get a ``TypeError``:

>>> class C:pass
... 
>>> class Widget2(TOSWidget, C): #doctest: +ELLIPSIS
...     pass
...
Traceback (most recent call last):
  ...
TypeError: Multiple inheritance of bases (<class '__main__.TOSWidget'>, <class __main__.C at 0x...>) is forbidden for TOS classes

This behavior is intentional: with this restriction you can simulate
an ideal world in which Python did not support multiple
inheritance. Suppose you want to claim that supporting multiple
inheritance was a mistake and that Python would have been better
without it (which is the position I tend to have nowadays, but see the
note below): how can you prove that claim? Simply by writing code that
does not use multiple inheritance and it is clearer and more
mantainable that code using multiple inheritance. I am releasing this
trait implementation hoping you will help me to prove (or possibly
disprove) the point.  You may see traits as a restricted form of
multiple inheritance without method resolution order and without name
clashes which does not pollute the namespace of the original class; it
does not have cooperative methods either. Still I think these are
acceptable restrictions since they give back in return many
advantages:

1. many people use multiple inheritance incorrectly, confusing the ``is-a``
relation with the ``has-a`` relation; with traits, there is no confusion,
since the features coming
by the base class correspond to ``is-a``, whereas the features coming
from the traits correspond to ``has-a``.

2. ``super`` becomes trivial, since each class has a single superclass.

Are there disadvantages of my proposed trait implementation with
respect to multiple inheritance? I don't think there are serious
disadvantages, since you can always work around them. For instance, a nice
property of inheritance is that if you have a class ``C`` inheriting from
class ``M`` and you change a method in ``M`` at runtime, after ``C`` has been
created and instantiated, automagically all instances of ``C`` gets the
new version of the method (this is pretty useful for debugging
purposes). This feature is not lost: the trait implementation
is fully dynamic and if you change the mixin the instances will
be changed too.

*Note*: even if I think that a language would be better off without
full multiple inheritance with cooperative methods, that does not mean
that I am against interfaces. I think a class should be able to
implement multiple interfaces at the same time, but this has nothing
to do with multiple inheritance. In Python instead (starting from
Python 2.6) multiple inheritance is abused to simulate interface
requirements so that if you want to implement many interfaces at the
same time you are suggested to inherit from many abstract base classes
at the same time.  In an ideal language without multiple inheritance
you could just add an ``__interfaces__`` attribute to classes. In an
ideal language interfaces would be abstract objects (a little more
than a list of names) whereas in Python they are concrete classes and
that explain why you inherit from them.

Introspection
------------------------------------------------------------------

As I said, a trait implementation must provide introspection facilities.
To this aim, ``mtrait`` provides a ``get_traits(obj)`` function returning
a ``TraitContainer`` if ``obj`` has a ``__traits__`` attribute or the
empty tuple otherwise. For instance

>>> #get_traits(label) is label.__traits__

>>> #get_traits(cls=TOSWidget) is TOSWidget.__traits__

>>> TOSWidget.__traits__
<Traits Grid, Place, Pack bound to <class '__main__.TOSWidget'>>

>>> label.__traits__ #doctest: +ELLIPSIS
<Traits Grid, Place, Pack bound to <__main__.TOSWidget object at 0x...>>

Future work
--------------------------------------------------------

The Smalltalk implementation of traits provides method renaming 
out of the box. The Python implementation has no facilities in
this sense. In the future I may decide to give some support for
renaming, or I may not. At the present I am not sure renaming is a good
idea: after all, it is pretty easy to create a new trait with different
names. For instance I can rename two read/write methods 

$$ReadWriteMixin

as dump/restore methods quite easily:

$$DumpRestoreMixin

Also, in the future I may decide to add some kind of adaptation mechanism
or I may not: after all the primary goal of this implementation is semplicity
and I don't want to clutter it with too many features.

I am very open to feedback and criticism: I am releasing this module with
the hope that it will be used in real life situations to gather experience
with the traits concept. Clearly I am not proposing that Python should
remove multiple inheritance in favor of traits: that will never happen.
I am just looking for a few adventurous volunteers wanting to experiment
with traits; if the experiment goes well, and people start using (multiple)
inheritance less than they do now, I will be happy. The point I am
trying to make is that Python is not Java: whereas in Java you have
very little alternatives to inheritance, in Python you have lots of
alternatives involving composition,
so you should not keep programming Java in Python.

.. _traits: http://www.iam.unibe.ch/~scg/Research/Traits/
.. _Traits - Composable Units of Behavior: http://www.iam.unibe.ch/%7Escg/Archive/Papers/Scha03aTraits.pdf
.. _PLT Scheme: http://www.cs.utah.edu/plt/publications/aplas06-fff.pdf

"""
import cPickle as pickle
from mtrait import *
from Tkinter import *

class TOSWidget(BaseWidget):
    include(Pack, Place, Grid)
    info = Pack.info.im_func
    config = Pack.config.im_func
    configure = Pack.configure.im_func
    slaves = Pack.slaves.im_func
    forget = Pack.forget.im_func
    propagate = Pack.propagate.im_func

class TOSWidget2(BaseWidget):
    __mixins__ = Pack, Place, Grid
    __metaclass__ = TOSMeta
    info = Pack.info.im_func
    config = Pack.config.im_func
    configure = Pack.configure.im_func
    slaves = Pack.slaves.im_func
    forget = Pack.forget.im_func
    propagate = Pack.propagate.im_func

label = TOSWidget(master=None, widgetName='label', cnf=dict(text="hello"))

pickle.dumps(label)

class HTTP(object):
    def GET(self):
        print 'calling HTTP.GET from %s' % self
    def POST(self):
        print 'calling HTTP.POST from %s' % self
    @classmethod
    def cm(cls):
        print 'calling HTTP.cm from %s' % cls

class FTP(object):
    def SEND(self):
        print 'calling FTP.SEND from %s' % self
    def RECV(self):
        print 'calling FTP.RECV from %s' % self
    @staticmethod
    def sm():
        print 'calling staticmethod'

class Mixin(object):
    def helper(self):
        return 1
    def method(self):
        return self.helper() + 1

class Meta(type):
    pass

def test_getattr():
    class C:
        __metaclass__ = TOSMeta
        def __getattr__(self, name):
            pass

def test_multi_include():
    class B(object):
        __metaclass__ = Meta
        include(FTP)
    class C(B):
        include(HTTP)
    print type(C)

def test_Trait_pickle():
    t = Trait(Mixin, Mixin.__name__)
    pickle.loads(pickle.dumps(t))

class C:
    "An example not using include"
    __metaclass__ = TOSMeta
    __mixins__ = HTTP, FTP, Mixin

class ReadWriteMixin(object):
   def read():
      pass
   def write():
      pass

class DumpRestoreMixin(object):
    dump = ReadWriteMixin.write.im_func
    restore = ReadWriteMixin.read.im_func

def setup():
    global c
    c = C()
    print c

class A(object):
    def save(self):
        print 'A.save'

class B(A):
    def save(self):
        print 'B.save'
        super(B, self).save()

class C(A):
    def save(self):
        print 'C.save'
        Super(self).save()

class D(B, C):
    def save(self):
        print 'D.save'
        super(D, self).save()

C.C_save = C.save.im_func

class E(B):
    include(C)
    def save(self):
        print 'D.save'
        print 'C.save'
        B.save(self)

check_overridden([B, C], set(), raise_="warning")

d = D()
e = E()
e.save()

if __name__ == '__main__':
    import doctest; doctest.testmod()
    try:        
        import nose
    except ImportError:
        pass
    else:
        nose.runmodule()
