'''\
The idea of creating classes by composing reusable collections of
methods is quite old: for instance Flavors_, and old Lisp dialect,
featured mixins more that 25 years ago.  Nevertheless, there are
still people thinking that the idea is new and cool: as a consequence,
we see a lot of new code where the idea is misused or
abused. Writing a paper showing the downfalls of mixins is
in my opinion worth the effort.

Injecting methods into a class namespace is a bad idea for a very
simple reason: every time you use a mixin,
you are actually polluting your class namespace and loosing track of the
origin of your methods. In this sense, using a mixin in a class is just
as bad as using ``from module import *`` in a module.
However, everybody agrees that it is much better to
use the full form ``mymodule.myname`` instead of importing all
the names in the global namespace, but nobody realizes that
importing methods into a class via a mixin is potentially just as bad.

At worst, using mixins can become
just a modern way of writing spaghetti code.
In order to explain what the problem is, I will show two real life
Python frameworks making use of mixins: Tkinter e Zope.

Mixins in real life
--------------------------------------------------------------

Tkinter_ is a GUI framework which is part of Python standard library.
It is a case where mixins work decently well, because it is a small
framework, but it is also large enough that it is possible to see the
beginning of the problem.

Every Tkinter class, even the simple ``Label`` is composed by multiple
mixins:

 >>> import Tkinter, inspect
 >>> for i, cls in enumerate(inspect.getmro(Tkinter.Label)): 
 ...     # show the ancestors
 ...     print i, cls
 0 Tkinter.Label
 1 Tkinter.Widget
 2 Tkinter.BaseWidget
 3 Tkinter.Misc
 4 Tkinter.Pack
 5 Tkinter.Place
 6 Tkinter.Grid

The standard library function ``inspect.getmro(cls)``
returns a tuple with the ancestors of ``cls``, in the order specified by
the Method Resolution Order (MRO). In our example the MRO of
``Label`` contains the geometry mixins (``Grid``, ``Pack`` and ``Place``)
and the generic mixin ``Misc`` which provides a lot of functionality
by delegating to the underlying Tk library.
The classes ``BaseWidget``, ``Widget``
and ``Label`` have state and they take the role of base
classes, not mixins.

.. figure:: http://www.phyast.pitt.edu/~micheles/python/Label.png

You should see what I mean by *namespace pollution*: if you use any
IDE with an autocompletion feature (or even ipython): if you try to
complete the expression ``Tkinter.Label.``,  you will get 181 choices.
181 attributes on a single class are *a lot*. If you invoke the builtin
help

  >>> help(Tkinter.Label)

you will see the origin of the different attributes, i.e. the classes
from which they come: the output spans a few hundreds of lines.

Luckily, Tkinter is a very stable framework (I mean: it works)
and there is no need to investigate the hierarchies to find bugs
or the reason for unexpected behavior. Moreover Tkinter is a comparatively
small framework: 181 attributes are many, but not too many, and with some
effort one could manage to find out their origin. Things are however very
much different in the case of Zope/Plone.

For instance, have a look at the hierarchy of the `Plone Site`_ class
which I report in appendix. Between square backets you can see the
number of methods/attributes defined per class, except special attributes.
The plot comes from
a real Plone application I have in production. The total count is of
38 classes, 88 names overridden, 42 special names and 648 regular
names: a monster.

To trace the origin of the methods and to keep in mind the hierarchy
is practically impossibile. Moreover, both autocompletion and the builtin
help facility become unusable, the self-generated class documentation
become unreadable since too big.

In other words, a design based on mixins works for small frameworks, but
it does not scale at all to large frameworks. Actually, Zope 3 has
been entirely rewritten with the goal of avoiding the mixin abuse of
Zope 2 and to use composition instead of inheritance (this is basically
what the buzzwords `Component Architecture`_ really mean).

My *hate* for mixins comes from my experience with Zope/Plone.
However the same abuses could be equally be done in other languages and
object systems - with the notable exception of CLOS, where methods are defined
outside classes and therefore the problem of class namespace pollution
does not exist - in the presence of huge frameworks.

A consequence of namespace pollution is that it is very easy to have
name clashes. Since there are hundreds of methods and it is impossible
to know all of them, and since method overriding is silent, this is
a real problem: the *very first time* I subclassed a Plone class I run
into this issue: I overrode a pre-defined method inadvertently, by causing
hard to investigate problems in an unrelated part of the code.

.. _Tkinter: http://www.pythonware.com/library/tkinter/introduction/x275-mixins.htm
.. _Python MRO: http://www.python.org/download/releases/2.3/mro/
.. _Python: http://www.python.org
.. _Ruby: http://www.ruby-lang.org/en/
.. _Scala: http://www.scala-lang.org/
.. _Multiple inheritance: http://en.wikipedia.org/wiki/Multiple_inheritance
.. _mixins: http://en.wikipedia.org/wiki/Mixin
.. _traits: http://www.iam.unibe.ch/~scg/Archive/Papers/Scha03aTraits.pdf
.. _Flavors: http://en.wikipedia.org/wiki/Flavors_(computer_science)
.. _Plone Site: http://www.phyast.pitt.edu/~micheles/python/plone-hierarchy.png
.. _Component Architecture: http://www.muthukadan.net/docs/zca.html

How to avoid accidental overriding
------------------------------------------------------------------------

The first thing I did after being bitten by Plone was to write an
utility function to identify the overridden methods. Let me show
here a simplified version of that function, called ``warn_overriding``.

You can use it when you need to work with a big framework which
you do not know well.

First of all, it is convenient to introduce a couple of utility functions:
``getpublicnames`` to get the public names in a namespace

$$getpublicnames

and ``find_common_names`` to extract the common names from a set of
classes:

$$find_common_names

Moreover, it is convenient to define a warning class:

$$OverridingWarning

Now it is easy to implement ``warn_overriding`` as a class decorator.

$$warn_overriding

Here is an example to show how it works in practice.
Given the base classes

$$Base
$$M1
$$M2

we can define the subclass

$$Child

which features method overriding. The overriding is specified by the MRO,
which includes five classes:


 >>> inspect.getmro(Child)
 (<class "Child">, <class "Base">, <class "M1">, <class "M2">, <type "object">)

``find_common_names`` takes those classes
(except ``object`` which does not provide any public name)
and look for common names which are printed by
``warn_overriding``:

 >>> Child = warn_overriding(Child)
 OverridingWarning: Child.{m1, spam} overriding Base.{m1, spam}
 OverridingWarning: Child.ham overriding M1.ham
 OverridingWarning: Child.spam overriding M2.spam
 OverridingWarning: Base.spam overriding M2.spam

In recent versions of Python (2.6+) it is possible to use
the elegant syntax

::

 @warn_overriding
 class Child(Base, M1, M2):
     ...

The advantages of the class decorator syntax are clear: the
decorator is much more visible since it comes *before* the class
and not after; moreover the warning prints the line number
corresponding to the class definition, the right place where
to look in case of overriding. It is possible to avoid the warnings
by listing explicitly the overriding methods in the ``.override``
class attribute. For instance, try to add the line::

     override = ['m1', 'spam', 'ham'] 

in the definition of ``Child`` and you will see that the warnings
will disappear

``warn_overriding`` is a small tool which can help you when you are fighting
with a big framework, but it is not a solution. The solution is not to
use mixins in the first place. The next articles in this series will
discuss a few alternatives.

Appendix: the hierarchy of Plone Site
---------------------------------------------

Here is the picture: if it is too big to fit in your screen, that
proves my point against mixins ;)

.. figure::  http://www.phyast.pitt.edu/~micheles/python/plone-hierarchy.png
'''

import inspect, warnings

class OverridingWarning(Warning):
    pass

def getpublicnames(obj):
    "Return the public names in obj.__dict__"
    return set(n for n in vars(obj) if not n.startswith('_'))

def find_common_names(classes):
    "Perform n*(n-1)/2 namespace overlapping checks on a set of n classes"
    n = len(classes)
    names = map(getpublicnames, classes)
    for i in range(0, n):
        for j in range(i+1, n):
            ci, cj = classes[i], classes[j]
            common = names[i] & names[j]
            if common:
                yield common, ci, cj

def warn_overriding(cls):
    """
    Print a warning for each public name which is overridden in the class
    hierarchy, unless if is listed in the "override" class attribute.
    """
    override = set(vars(cls).get("override", []))
    ancestors = inspect.getmro(cls)
    if ancestors[-1] is object: # remove the trivial ancestor <object>
        ancestors = ancestors[:-1]
    for common, c1, c2 in find_common_names(ancestors):
        overridden = ', '.join(common - override)
        if ',' in overridden: # for better display of the names
            overridden = '{%s}' % overridden
        if overridden:
            msg = '%s.%s overriding %s.%s' % (
                c1.__name__, overridden, c2.__name__, overridden)
            warnings.warn(msg, OverridingWarning, stacklevel=2)
    return cls

def check_disjoint(bases):
    for common, b1, b2 in find_common_names(bases):
        raise NameError(
            'Found common name(s) between %s and %s: %s' %
            (b1.__name__, b2.__name__, ', '.join(common)))

class Base(object):
    def m1(self):
        pass
    def spam(self):
        pass

class M1(object):
    def m2(self):
        pass
    def ham(self):
        pass

class M2(object):
    def m3(self):
        pass
    def spam(self):
        pass

#class C(Base, M2): pass
#warn_overriding(C)

#@warn_overriding
class Child(Base, M1, M2):
    def ham(self):
        pass
    def spam(self):
        pass
    def m1(self):
        pass
warn_overriding(Child)
