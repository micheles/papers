r"""An implementation of traits in Python
==================================================================

:Author: Michele Simionato
:Date: XXX
:Version: XXX
:Download: XXX
:Licence: BSD
:Status: XXX
:Abstract: 

 *The mtrait module provides an implementation of 
 traits as units of composable behavior for Python. It is
 argued that traits are better than multiple inheritance.
 Implementing frameworks based on traits is left as an exercise
 for the reader.*

Motivation
------------------------------------------------

Multiple inheritance is a hotly debated topic. 
The supporters of multiple inheritance 
claim that it makes code shorter and easier
to read, whereas the opposers claim that is makes
code more coupled and more difficult to understand.  I have
spent some time in the past facing the intricacies of `multiple
inheritance in Python`_ and I was one of its supporters once; however,
since then I have worked with frameworks making
large use of multiple inheritance (I mean Zope 2) and nowadays I am in
the number of the people who oppose it.  Therefore I
am interested in alternatives.

In recent years, the approach
of traits_ has gained some traction in a few circles and I have
decided to write a library to implement traits in Python, for
experimentation purposes. The library is meant for framework builders,
people who are thinking about writing a framework based on multiple
inheritance - typically via the common mixin approach - but
are not convinced that this is the best solution and would like to try
an alternative. This library is also for authors of mixin-bases frameworks
which are unsatisfied and would like to convert their
framework to traits.

Are traits a better solution than multiple inheritance and mixins?  In
theory I think so, otherwise I would not have written this library, but
in practice (as always) things may be different. It may well be 
in practice that using traits or using mixins does not make a big
difference and that the change of paradigm is not worth the effort; or the
opposite may be true. The only way to know is to try, to build
software based on traits and to see how it scale *in the large*.
In the small, of course, more or less any approach works fine: it is only
by programming in the large that you can see the differences.  

This is
the reason why I am releasing this library with a liberal licence, so
that people can try it out and see how it works. The library is meant
to play well (when possible) with pre-existing frameworks. 
As an example, I will show
here how you could rewrite Tkinter classes to use traits instead of mixins. Of
course, I am not advocating rewriting Tkinter: it would be silly 
and pointless; but it may have sense (or
not) to rewrite your own framework using traits, perhaps a framework
which is used in house but has not been released yet.

I am not the only one to have
implemented traits for Python; after finishing my implementation
I made a little research and discovered a few implementations. The
biggest effort seems to be `Enthought Traits`_ which however is
large implementation and seems to use the name to intend something very
different (i.e. a sort of type checking). My implementation has no
dependencies, is short and I am committed
to keep it short even in the future, according to
the principle of `less is more`_. 

There is also an hidden agenda behind this module: to popularize some
advanced features of the Python object model which are little
known. The ``mtrait`` module is actually a tribute to the
metaprogramming capabilities of Python: such features are usually
associated to languages with a strong academic tradition - Smalltalk,
Scheme, Lisp - but actually the Python object model is no less
powerful.  For instance, changing the object system from a multiple
inheritance one to a trait-based one, with *different* lookup rules,
can be done *within* the fundamental object system. The reason is that
the features that Guido used to implement the object system (special
method hooks, descriptors, metaclasses) are there, available to the
end user to build her own object system.

Such features are usually little used in the Python community, for
many good reasons: the object system is good enough as it is and there
is no reason to change it; moreover there is a strong opposition to
change the language, because Python programmers believe in uniformity
and in using common idioms; finally, it is difficult for application
programmer to find a domain where these features are useful. An
exception is the domain of the Object Relation Mappers, whereas the
Python language is often stretched to mimic the SQL language, a famous
example of this tendency being SQLAlchemy_).
Still, I have never seen a perversion of the object model as big
as the one implemented in the ``mtrait`` module, so I wanted to
be the first one to perform that kind of abuse ;) 

.. _multiple inheritance in Python: MRO
.. _less is more: http://www.artima.com/weblogs/viewpost.jsp?thread=236286
.. _Enthought Traits: https://svn.enthought.com/enthought/wiki/Traits

What are traits?
------------------------------------------------------------

The word *traits* has many meanings; I will refer to it in the sense
of the paper `Traits - Composable Units of Behavior`_ which implements
them in Squeak/Smalltalk. The paper appeared in 2003, but most of the
ideas underlying traits have been floating around for at least 30
years. There is also a trait implementation for `PLT Scheme`_ which is
somewhat close in spirit (if not in practice) to what I am advocating here.
The library you are reading about is by no means intended as a porting
of the Smalltalk library: I am just stealing some of the ideas from
that paper to implement a Pythonic alternative to mixins which, for
lack of a better name, I have decided to call traits. I feel no
obligation whatsoever to be consistent with the Smalltalk library. In
doing so, I am following a long tradition, since a lot of languages
use the name *traits* to mean something completely different from the
Smalltalk meaning. For instance the languages Fortress and Scala use
the name *trait* but they mean by it what is usually called a *mixin*.
For me a trait is a bunch of methods and attributes with the following
properties:

1. the methods/attributes in a trait go logically together;
2. if a trait enhances a class, then all subclasses are enhanced too;
3. if a trait has methods in common with the class, then the
   methods defined in the class have the precedence;
4. the ordering of traits is not important, i.e. enhancing a class 
   first with trait T1 and then with trait T2 or viceversa is the same;
5. if traits T1 and T2 have names in common, enhancing a class both
   with T1 and T2 raises an error;
6. if a trait has methods in common with the base class, then the
   trait methods have the precedence;
7. a class can be seen both as a composition of traits and as an homogeneous
   entity.

Properties from 4 to 7 are the distinguishing properties of traits
with respect to multiple inheritance and mixins.  In particular,
because of 4 and 5, all the complications with the Method Resolution
Order disappear and the overriding is never implicit.  Property 6 is
mostly unusual: typically in Python the base class has the precedence
over mixin classes.  Property 7 has to be intended in the sense that a
trait implementation must provide introspection facilities to make
seemless the transition between classes viewed as atomic entities and
as composed entities.

A hands-on example
------------------------------------------------------

Let me begin by showing how you could rewrite a
Tkinter class to use traits instead of mixins. Consider the
``Tkinter.Widget`` class, which is derived by the base class
``BaseWidget`` and the mixin classes
``Tkinter.Grid``, ``Tkinter.Pack`` and ``Tkinter.Place``: we want to
rewrite it to use traits. The ``mtrait`` module
provides a class decorator named ``include`` to do the job; it is
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

 class Widget(BaseWidget): # this syntax works is backward-compatible
     include(Pack, Place, Grid)

The preferred way however is to use the ``@`` notation, if available.
I said that the conversion from mixins to traits was easy: but actually
I lied since if you try to execute the code I just wrote you will
get an ``OverridingError``:

.. code-block:: python

 >>> from Tkinter import *
 >>> class Widget(BaseWidget):
 ...     include(Pack, Place, Grid)
 Traceback (most recent call last):
   ...
 OverridingError: Pack overrides names in Place: {info, config, configure, slaves, forget}

The reason for the error is clear: both ``Pack`` and ``Place`` provide
methods called ``{info, config, configure, slaves, forget}`` 
and the traits implementation cannot figure out
which ones to use.  This is a feature, since it forces you to be
explicit. In this case, if we want to be consistent with
multiple inheritance rules, clearly we want the methods coming from
the first class (i.e. ``Pack``) to have precedence. That can be
implemented by including directly those methods in the class namespace
and relying on rule 3:

$$TOSWidget

Notice that we had to specify the ``propagate`` method too, since
it is common between ``Pack`` and ``Grid``.

You can check that the TOSWidget class works, for instance by defining a
label widget as follows (remember that ``TOSWidget`` inherits its signature
from  ``BaseWidget``):

.. code-block:: python

 >>> label = TOSWidget(master=None, widgetName='label',
 ...                   cnf=dict(text="hello"))

You may visualize the widget by calling the ``.pack`` method:

.. code-block:: python

 >>> label.pack()

This should open a small window with the message "hello" inside it.

A few caveats
----------------------------------------------------

As you see, in a lot of cases replacing mixins with traits is fairly
straightforward: however, in some cases you can run into problems.
For instance, rule 6 implies an attribute lookup different from
the standard lookup. Consider the following example:

.. code-block:: python

 >>> class Base(object):
 ...     a = 1

 >>> class ATrait(object):
 ...     a = 2

 >>> class Class(Base):
 ...     include(ATrait)

 >>> Class.a
 2

In regular multiple inheritance instead the base class attribute
would have the precedence:

.. code-block:: python

 >>> type('Class', (Base, ATrait), {}).a
 1

Therefore replacing mixin classes with traits can break your code if
you rely on the standard overriding rules. Be careful!

Also, you should be aware of the fact that *special methods
are special*: the ``mtrait`` module perverts the Python object system
to follow rules 1-7 for all attribute access *except* for special
attributes. Special attributes of the form ``__xxx___`` in a trait
are just *ignored*:

.. code-block:: python

 >>> ATrait.__special__ = 3 # this attribute is NOT transmitted to Class
 >>> hasattr(Class, '__special__')
 False

Finally, if you have a cooperative multiple inheritance hierarchy
(i.e. you use ``super`` in the mixin classes) you will have to rewrite
your code not to use ``super`` (notice however that there are no
problem if you use ``super`` ony in methods of the base class). This
is on purpose: traits are not a replacement for cooperative multiple
inheritance, you should uses traits if you think that cooperative
multiple inheritance is a bad idea and you are willing to remove it
from your code. Old frameworks written before the introduction of
``super`` (such as Tkinter and Zope 2) are easier to convert in this
respect, since they are non-cooperative.  The ``mtrait`` module is
intended for framework writers, so it assumes you can change the
source code of your framework if you want; of course, it aims to
reduce the needed changes as much as possible.

How does it work
---------------------------------------------------------

The implementation of traits provided is an ``mtrait`` is short (under
300 lines of code including comments and docstrings) but if makes use
of fairly sophisticated techniques. The building block is a
``Trait`` class which you can use directly, instead of relying on the
magic of ``include``. You can see a trait as a wrapper around an inner
object.  Usually you want to convert a mixin class into a trait; you
can do it as follows:

  ``trait = Trait(Mixin, Mixin.__name__)``

It is also common to convert a module into a trait:

  ``trait = Trait(module, module.__name__)``

Trait objects are attribute descriptors, i.e. they can used as class
attributes; when a trait is called from a class or an instance, we
say that it is bound to the class or the instance.

Here is an example of usage:

.. code-block:: python

 >>> pack = Trait(Pack, 'Pack') # define an unbound trait
 >>> pack
 <Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} >

 >>> class MyWidget(BaseWidget): # add the trait to a class
 ...      p = pack

Here is an example of trait a bound to a class:

.. code-block:: python

 >>> MyWidget.p #doctest: +ELLIPSIS
 <Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} bound to <class __main__.MyWidget at 0x...>>

Here is an example of a trait bound to an instance:

.. code-block:: python

 >>> lbl = MyWidget(None, 'label', dict(text='hello'))
 >>> lbl.p #doctest: +ELLIPSIS
 <Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} bound to <__main__.MyWidget instance at 0x...>>

You can call the methods of a trait as follows:

.. code-block:: python

 >>> lbl.p.pack()
 >>> lbl.p.info().keys()
 ['side', 'ipady', 'ipadx', 'in', 'pady', 'padx', 'anchor', 'expand', 'fill']

The descriptor magic works in such a way that the instance methods get
as first argument the instance to which the trait is bound; on the other
hand, for traits bound to classes, you get an unbound method associated
to the given class:

.. code-block:: python

 >>> MyWidget.p.pack
 <unbound method MyWidget.pack_configure>

Different traits or mixins can be composed into a ``TraitContainer``:
for instance, you could define

.. code-block:: python

 >>> tc = TraitContainer.from_([Pack, Place, Grid])
 >>> tc
 <Traits Grid, Place, Pack >

The ``TraitContainer`` object has attributes ``Pack``, ``Place`` and ``Grid``
which are traits corresponding to the mixin classes; for instance

.. code-block:: python

 >>> tc.Pack
 <Trait Pack {config, configure, forget, info, pack, pack_configure, pack_forget, pack_info, pack_propagate, pack_slaves, propagate, slaves} >

Internally the ``include`` decorator builds a trait object for each mixin and
store all of them into the ``__traits__`` attribute descriptor,
making your traits ready for introspection (rule 7):

.. code-block:: python

 >>> TOSWidget.__traits__ # bound-to-class TraitContainer
 <Traits Grid, Place, Pack bound to <class '__main__.TOSWidget'>>

 >>> label.__traits__ #doctest: +ELLIPSIS
 <Traits Grid, Place, Pack bound to <__main__.TOSWidget object at 0x...>>

Traits play well with pydoc, ipython and the other tools used
to introspect Python objects: try to type
``help(lbl)`` and you will see for yourself how it works.

The Trait Object System
----------------------------------------------------------------------

The goal of the ``mtrait`` module it to modify the standard
Python object model, turning it into a Trait Object System (TOS for short): 
TOS classes behave differently from regular
classes. In particular TOS classes must not support multiple inheritance
and must forbid your from defining your own ``__getattribute__`` and 
``__getstate__`` methods; these
two fundamental properties must be preserved under inheritance (i.e. the
son of a TOS class must be a TOS class) and therefore the implementation
necessarily requires metaclasses.
Internally the ``include`` class decorator works
its magic by changing the metaclass of the original class. Usually the
metaclass is changed to ``TOSMeta``, the basic class of the Trait
Object System. However in general not all TOS classes are
instances of ``TOSMeta``. A class is a TOS class if it satisfies
a given interface: the ``mtrait`` module provides a ``isTOSclass``
utility function which performs the check:

$$isTOSclass

The check is *intentionally* loose, i.e. you can fool it by
setting a fake ``__traits__`` attribute: it is there to
prevent accidental mistakes, not to give any guarantee.
On the other hand,
the ``include`` class decorator ensures that the metaclass of the
decorated class is a subclass of the metaclass of the undecorated
class; moreover, it adds the proper ``__traits__``,
``__getattribute__``, ``__getstate__`` and ``__mixins__`` attributes,
doing the same job as ``TOSMeta`` even if it is not necessarily a
subclass of ``TOSMeta``.

``TOSMeta`` does a lot of work: in particular it adds a
suitable ``__getattribute__`` method to its instances.
We need to override the
``__getattribute__`` method 
since we want to change the attribute lookup rules: in regular
Python, the usual rules are look at the class, look at
the base class and look at ``__getattr__``; rules 6 instead
says that traits must have the precedence over the base class,
so overriding ``__getattr__`` would not be enough. Fortunately,
the Python object model is powerful enough to allows users
to change the rules of the game: by overriding ``__getattribute__``
it is possible to lookup at the traits attributes *before* looking
at the base class. Notice that is necessary to override
``__getattribute__`` both at the class level and at the metaclass
level, to be able to manage both instance attributes and class
attributes.

``TOSMeta`` also adds a suitable ``__getstate_``
method so that your objects stay pickleable if they were originally
pickleable (adding ``__getattribute__`` without adding ``__getstate__`` would
break pickle). Notice that the original class should not define
``__getattribute__`` or ``__getstate__`` of its own, otherwise an 
``OverridingError`` is raised. This is intended to prevent
accidental overriding; if you really know what you are doing,
you can always replace the default implementation with your own
after class creation.

People wanting to do everything explicitely can use
the ``__metaclass__`` hook and specify the mixins directly with ``__mixins__``:

$$TOSWidget2

The magic of ``include``
------------------------------------------------------

``include`` does more than just dispatching to the ``__metaclass__`` hook: it
takes care automatically of possible metaclass conflicts. In the case of
Tkinter there is no such problem, since ``BaseWidget`` is just a traditional
old-style class and the metaclass for ``TOSWidget2`` is just ``TOSMeta``:

.. code-block:: python

 >>> type(TOSWidget)
 <class 'mtrait.TOSMeta'>

Nevertheless, in general you may need to build your Trait Based Framework
on top of pre-existing classes with a nontrivial metaclass, for
instance Zope classes; in that case having a class decorator smart
enough to figure out the right metaclass to use is a convenient
facility. Here is an example:

$$AddGreetings

$$PackWidget

The ``include`` decorator automatically generates the right metaclass as
a subclass of ``AddGreetings``:

.. code-block:: python

 >>> print type(PackWidget).__mro__
 (<class 'mtrait._TOSMetaAddGreetings'>, <class '__main__.AddGreetings'>, <type 'type'>, <type 'object'>)
 
This trick avoids the dreaded `metaclass conflict`_. ``_TOSMetaAddGreetings``
provides the same features of ``TOSMeta`` and in particular you may
check that it sets the ``__traits__`` attribute correctly:

 >>> PackWidget.__traits__
 <Traits Pack bound to <class '__main__.PackWidget'>>

On the other hand, ``_TOSMetaAddGreetings`` is a subclass of ``AddGreetings``
which calls ``AddGreetings.__new__``, so the features provided by
``AddGreetings`` are not lost either; in this example you may check
that the greetings attribute is correctly set:

 >>> PackWidget.greetings
 'hello!'

The name of the generated metaclass
is automatically generated from the name of the base
metaclass; moreover, a register of the generated metaclasses
is kept, so that metaclasses are reused if possible.
If you want to understand the details, you are welcome
to give a look at the implementation, which is pretty short
and simple, compared to the general recipe to remove
the `metaclass conflict`_ in a true multiple inheritance situation.

.. _sqlalchemy: http://www.sqlalchemy.org/
.. _metaclass conflict: http://code.activestate.com/recipes/204197/

Why multiple inheritance is forbidden
----------------------------------------------------------

As I said, the mother metaclass of the Trait Object System ``TOSMeta`` forbids
multiple inheritance and if you try to multiple inherit from a TOS
class and another class you will get a ``TypeError``:

.. code-block:: python

 >>> class M:
 ...     "An empty mixin"
 ...
 >>> class Widget2(TOSWidget, M): #doctest: +ELLIPSIS
 ...     pass
 ...
 Traceback (most recent call last):
   ...
 TypeError: Multiple inheritance of bases (<class '__main__.TOSWidget'>, <class __main__.M at 0x...>) is forbidden for TOS classes

This behavior is intentional: with this restriction you can simulate
an ideal world in which Python did not support multiple
inheritance. Suppose you want to claim that supporting multiple
inheritance was a mistake and that Python would have been better off
without it (which is the position I tend to have nowadays): how can
you prove that claim? Simply by writing code that does not use
multiple inheritance and it is clearer and more mantainable that code
using multiple inheritance.

I am releasing this trait implementation hoping you will help me to
prove (or possibly disprove) the point.  You may see traits as a
restricted form of multiple inheritance without method resolution
order and without name clashes which does not pollute the namespace of
the original class; it does not have cooperative methods either. Still
I think these are acceptable restrictions since they give back in
return many advantages in terms of simplicity: for instance, ``super``
becomes trivial, since each class has a single superclass, whereas
we all know that the `current
super in Python`_ is very far from trivial. 

More importantly, many people use multiple inheritance incorrectly,
confusing the ``is-a`` relation with the ``has-a`` relation; with
traits, there is no confusion. Since there is a single base class, you
can associate the ``is-a`` relation with the base class whereas the
features coming from the traits correspond to ``has-a``: for instance
in the Tkinter example a ``Widget`` *is* a ``BaseWidget`` but has the
methods of the traits ``Pack``, ``Place`` and ``Grid``.

One could take even a more extreme stance: abolish both multiple inheritance
and traits and add methods to a class by hand, which something
like this:

.. code-block:: python

  def include_methods(cls, method_container):
     for name, value in vars(method_container).itervalues():
         setattr(cls, name, value)

This approach is definitely simple, but perhaps *too* simple.  A nice
property of inheritance is that if you have a class ``C`` inheriting
from class ``M`` and you change a method in ``M`` at runtime, after
``C`` has been created and instantiated, automagically all instances
of ``C`` gets the new version of the method, which is pretty useful
for debugging purposes. This feature is lost in the simplistic
approach but not lost in the trait approach: the trait implementation
is fully dynamic and if you change the mixin the instances will be
changed too. This however, is a minor issue (I never
used that feature much). 

The real reason why just including methods in the class namespace is a
bad idea is that it easily leads you into the problem of *namespace
pollution*. I have discussed the issue elsewhere_: if you keep
injecting methods into a class (both directly or via inheritance) you
may end up having hundreds of methods flattened at the same level. A
picture is worth a thousand words, so have a look at the `PloneSite
hierarchy`_ if you want to understand the horror I would like to avoid
with traits (the picture shows the number of nonspecial attributes
defined per class in square brackets): in the Plone Site hierarchy
there are 38 classes, 88 overridden names, 42 special names, 648
non-special attributes and methods. It is a nighmare. 

The ``mtrait`` implementation avoids name space pollution since each
trait has its own namespace and you are guaranteed against name
conflicts. Moreover, introspecting traits is much easier than
introspecting inheritance hierarchies: even the autocompletion feature
works best.

.. _current super in Python: http://www.artima.com/weblogs/viewpost.jsp?thread=236275
.. _elsewhere: http://stacktrace.it/articoli/2008/06/i-pericoli-della-programmazione-con-i-mixin1/
.. _PloneSite hierarchy: http://www.phyast.pitt.edu/~micheles/python/plone-hierarchy.png 


Discussion of some design decisions and future work
--------------------------------------------------------

The decision of having TOS classes which are not instances of TOSMeta
required some thinking. That was my original idea in version 0.1 of
``mtrait``; however in version 0.2 I wanted to see what would happen
if I made all TOS classes instances of TOSMeta. That implied that if
your original class had a nontrivial metaclass, then the TOS class had
to inherit both from the original metaclass *and* ``TOSMeta``,
i.e. multiple inheritance and cooperation of methods was required at
the metaclass level.  I did not like it, since I was arguing that
you can do everything without multiple inheritance and cooperative
methods; moreover using multiple inheritance at the metaclass level
meant that one had to solve the metaclass conflict in a general
way. I did so, by using my own cookbook recipe, and all my tests
passed.

Neverthess, at the end, in version 0.3 I decided to go back to the
original design. The metaclass conflict recipe is too complex, and I
see it as a code smell - *if the implementation is hard to explain,
it's a bad idea* - just another indication that multiple inheritance
is bad. In the original design it is possible to add
the features of ``TOSMeta`` to the original metaclass by subclassing
it with *single* inheritance and thus avoiding the conflict. 

The price to pay is that the TOS class is no more an instance of
``TOSMeta``, but this is a non-issue: the important thing is that TOS
classes perform the dispatch on their traits as ``TOSMeta`` would
dictate.  Moreover, starting from Python 2.6, thanks to `Abstract Base
Classes`_, you may satisfy the ``isinstance(obj, cls)`` check even if
``obj`` is not an instance of ``cls``, by registering a suitable base
class (similarly for ``issubclass``). In our situation, that means
that it is enough to register ``TOSMeta`` as base class of the
original metaclass.

Where to go from here? For the moment, I have no clear idea about the
future. The Smalltalk implementation of traits provides method
renaming out of the box. The Python implementation has no facilities
in this sense. In the future I may decide to give some support for
renaming, or I may not. At the present you can just rename your
methods by hand.  Also, in the future I may decide to add some kind of
adaptation mechanism or I may not: after all the primary goal of this
implementation is semplicity and I don't want to clutter it with too
many features.

I am very open to feedback and criticism: I am releasing this module
with the hope that it will be used in real life situations to gather
experience with the traits concept. Clearly I am not proposing that
Python should remove multiple inheritance in favor of traits. I am
just looking for a few adventurous volunteers wanting to experiment
with traits; if the experiment goes well, and people start using
(multiple) inheritance less than they do now, I will be happy. 

The point I am trying to make is that Python is not Java: whereas in
Java you have very little alternatives to inheritance, in Python you
have lots of alternatives involving composition and it is worth to
try them out.

*Post Scriptum:* if your are curious about the origin of the letter ``m`` in
``mtrait``, I added it to avoid conflicts with other traits
implementation; it stands for *Meta* or for *My* or even for
*Michele*, at your will ;)

.. _Abstract Base Classes: http://www.python.org/dev/peps/pep-3119/
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

def test_label():
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
    def _helper(self):
        return 1
    def method(self):
        return self._helper() + 1

class AddGreetings(type):
    "A metaclass a 'greetings' attribute for exemplification purposes"
    def __new__(mcl, name, bases, dic):
        dic['greetings'] = 'hello!'
        return super(AddGreetings, mcl).__new__(mcl, name, bases, dic)

class PackWidget(BaseWidget):
    __metaclass__ = AddGreetings
    include(Pack) # put this line AFTER the __metaclass__ hook!

def test_getattr():
    try:
        class C:
            __metaclass__ = TOSMeta
            def __getattribute__(self, name):
                pass
    except OverridingError: # expected
        pass
    else:
        raise RuntimeError("OverridingError not raised!")
    
def test_multi_include():
    class B(object):
        __metaclass__ = AddGreetings
        include(FTP)
    class C(B):
        include(HTTP)
        def __init__(self, a):
            self.a = a
    class D(C):
        pass
    x = D(1)
    x.a
    x.cm()
    x.sm()
    print type(B), type(C)

def test_Trait_pickle():
    t = Trait(Mixin, Mixin.__name__)
    pickle.loads(pickle.dumps(t))

class C:
    "An example not using include"
    __metaclass__ = TOSMeta
    __mixins__ = HTTP, FTP, Mixin

def setup():
    global c
    c = C()
    print c

class A(object):
    def save(self):
        print 'A.save'

class AM(A):
    include(Mixin)

assert AM().method() == 2

class B(A):
    def save(self):
        print 'B.save'
        super(B, self).save()

class C(A):
    def save(self):
        print 'C.save'
        A.save(self)

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

'''
The class decorator ``include`` takes a set of mixin
classes in input, determines the right metaclass to use and returns a
class in output; the metaclass enhances the class in output by adding
to it a special attribute ``__mixins__``, a descriptor ``__traits__``, a custom
``__getattribute__`` method and a custom ``__getstate__`` method. The
``__mixins__`` attribute is just the tuple of the included mixins;
the ``__traits__`` descriptor is a ``TraitContainer`` object akin to a
dictionary of ``Trait`` objects (one for each mixin class); the
``__getattribute__`` method allows automatic dispatching to them; the
``__getstate__`` method is needed to preserve pickleability of the
instances - assuming the original class before decoration was
pickleable, of course.

Are there disadvantages of my proposed trait implementation with
respect to multiple inheritance? I don't think there are serious
disadvantages, since you can always work around them.
*Multiple inheritance is most useful at the metaclass level,
where you need to cooperatively override __new__!*

from ms.tools.conceptualmap import *
PloneSite=app.rcare.__class__
plot_classes(PloneSite.mro(), verbose=2)


In theory you could build your trait objects yourself as explained in
the previous paragraph, and you could implement the dispatch to traits
by hand; however, in practice it is much easier if you just rely on the
magic of the  class decorator, which is doing a lot of work
on your behalf. In particular,


*Note 1*: Python has somewhat of a schizofrenic attitude towards inheritance.
Recent evolutions of the language

even if I think that a language would be better off without
full multiple inheritance with cooperative methods, that does not mean
that I am against interfaces. I think a class should be able to
implement multiple interfaces at the same time, but this has nothing
to do with multiple inheritance. However in Python (starting from
Python 2.6) multiple inheritance is abused to simulate interface
requirements so that if you want to implement many interfaces at the
same time you are suggested to inherit from many abstract base classes
at the same time.  In an ideal language without multiple inheritance
you could just add an ``__interfaces__`` attribute to classes. In an
ideal language interfaces would be abstract objects (a little more
than a list of names) whereas in Python they are concrete classes and
that explains why you inherit from them.
'''
