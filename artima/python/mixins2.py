'''
A few conceptual issues with mixins
----------------------------------------------------------------

In the first part of this series I have discussed a very serious
problem of the mixin approach, i.e. the namespace overpopulation issue.

.. figure:: http://www.phyast.pitt.edu/~micheles/python/Overpopulation.jpg

   The namespace overpopulation issue

Some reader hinted that this is a problem for large frameworks, but not
for small frameworks. Moreover, some argued that mixins *per se* are
a fine technique and that they can be used properly.
I am not really convinced since I believe there are better solutions.

First of all, let me state again that the idea of growing functionality
by adding more and more mixin classes is just plain wrong.
It is true that you can use the idea in little frameworks
with little damage, but that does not make it a good design solution.
Small frameworks have a tendency to grow, and you should not
start with a week design.

Moreover, I have a few conceptual issues with mixins - as implemented in most
languages - which are independent of the overpopulation problem.

I think everybody agrees that the best way to solve a
complex problem is to split it in smaller decoupled subproblems, by
following the *dividi et impera* principle. The disturbing thing about
mixins is that the principle is applied at the beginning (the problem
is decomposed in smaller independent units) but at the end
the different functionalies are added back to the client class as
an undifferentiated soup of methods.

Therefore a design based on mixins looks clean to the
framework writer - everything is well separated in his mind - but
it looks messy to the framework user - she sees
methods coming from all directions without a clear separation. It is
really the same situation than using the 
``from module import *`` idiom, which is rightly frowned upon because
if you use it names are separated in different namespaces
from the library author point of view, but they are all messed
up from the library user point of view.

Mixins make easier the life of the framework writer, but make more
difficult the life of the framework reader, and I find that mostly
unpythonic: the goal of Python is to make code *easy to read*, not
easy to write.
The scenario I have in mind is the usual one: a poor programmer who needs
to debug an object coming from a gigantic framework which is *terra
incognita* to her, without any documentation and with a strict
deadline (do you see yourself in there?). In such conditions a
framework heavily based on mixins makes things harder, since the usual
introspection techniques (autocompletion, pydoc, etc.)  fail and the
programmer gets drowned under hundreds of methods which are properly
ordered in mixin classes on the paper, but not on the battle field.

There is also another conceptual issue. The idea
behind mixins is that they should be used for generic functionality
which can be applied to different classes (think of mixins like Persistent,
Comparable, Printable, etc.). But this is exactly the same situation
where you want to use generic functions.

In this `post of mine`_ I actually argue that generic functions
(a.k.a. multimethods) are a better solution than mixins. I also provide
a very concrete example, which I think generalizes. The advantage of generic
functions is that they are clearly defined outside classes,
whereas the mixin approach is kind of schizophrenic:
the functionality is actually defined externally, but that fact
is made invisible to the final user.

I am a big fan of generic functions which are already used
a lot in the Python word - ``print`` is a generic function,
the comparison operators are generic functions, numpy_ universal
functions (ufunctions) are generic functions, etc - but should be
used even more. With generic functions, mixins becomes useless.
A side effect is that the class namespace becomes much slimmer: for instance,
in CLOS_ classes are used just to contain state, whereas the methods
live in a separate namespace. In most languages instead,
classes are used as a namespace control mechanism, performing
double duty - namespace control should be the job of modules.

.. _numpy: http://numpy.scipy.org/

A particularly bad usage of mixins
-----------------------------------------------------------

A tipical beginner's tutorial (for instance, I recommend `Using
Mix-ins with Python`_ , by Chuck Esterbrook which is very well written
and very informative, even if the point of view is exactly the
opposite of mine) will tell you that mixins are used to add
functionality to the classes they mix in. For instance a mixin class
``WithLog`` 
could be used to enhance a pre-existing class ``C`` with a logging
capability:

$$WithLog
    
$$C_WithLog

An example of usage is the following:

 >>> c = C_WithLog()
 >>> c.log.warn("hello")

That prints

 ``WARNING:C_WithLog:hello``.

This usage of mixins you see here is plain wrong: why would you use
inheritance when you need just one method? You can just import the one
method you need!  Generally speaking, a mixin class has sense only
when you have a set of methods which are logically together: if you
have a single method, or a set of disconnected methods, you are much
better off by defining the methods externally, in an utility module,
and then by importing them directly in the class namespace. Of course,
here I am assuming that you really want the external method to ends up
in the class namespace, possibly because of a interface requirement,
but I am not saying that this is a good idea. You can import the
method in your class as simply as that:

$$CWithLog

This approach is very little used in Python, probably because most people
coming from other languages does not know it is possible, but it
is in my opinion a much clearer solution than inheritance.
The problem with inheritance is that it requires a *substantial
cognitive load*: when I see the line of code ``class C_WithLog(C, WithLog)``
I see that ``WithLog`` is a class, and immediately I ask myself many
questions: which methods are exported by ``WithLog``?
is there any method of ``C`` which accidentally overrides one of the methods
of ``WithLog``? if yes, is there any method cooperation mechanism
(``super``) or not? what are the ancestors of ``WithLog``? which methods
are coming from them? are such methods overridden by some ``C`` method?
is there a cooperation mechanism on ``WithLog`` ancestors? What's the `method
resolution order`_ of the hierarchy?
On the other hand, if I see ``from utility import log`` I have very little
to understand and very little to worry about. The only caution in this
specific example is that I will have a single logger shared by all
instances of the class since
``logging.getLogger(self.__class__.__name__)`` will return always the
same object. If I need different loggers with different configurations
for different instances I will have to override the ``.log`` attribute on
a case by case basis, or I will have to use a different strategy, such as
the `dependency injection pattern`_.

.. _Using Mix-ins with Python: http://www.linuxjournal.com/article/4540
.. _dependency injection pattern: http://en.wikipedia.org/wiki/Dependency_injection
.. _CLOS: http://en.wikipedia.org/wiki/CLOS
.. _post of mine: http://www.artima.com/weblogs/viewpost.jsp?thread=237764
.. _method resolution order: http://www.python.org/download/releases/2.3/mro/

An acceptable usage of mixins
---------------------------------------------------------------

There are usages for mixins which are restricted in scope and not
dangerous: for instance, you can use mixins for implementing the comparison
interface, or the mapping interface.  This is actually the approach suggested
by the standard library, and by the new ABC's in Python 2.6. This is
an acceptable usage: in this case you are there is no incontrollable
growth of methods, since you are actually implementing well know
interfaces - typically a few well known special methods.
In order to give a practical example, let me discuss a toy application.

Suppose you want to define a ``PictureContainer`` class in an application
to manage pictures and photos. A ``PictureContainer`` object may contain
both pictures and ``PictureContainer`` objects, at any level of nesting.
From the point of view of the Python programmer it could make sense
to implement such a class by using a dictionary.
A ``Picture`` object will contain information such as the picture
title, the picture date, a few methods to read and write the
picture on the storage (the file system, a relation database,
an object database like the ZODB or the AppEngine datastore_,
or anything else).

.. _datastore: http://code.google.com/appengine/docs/datastore/
.. _ZODB: http://wiki.zope.org/ZODB/guide/index.html

The first version of the ``PictureContainer`` class could be something
like that:

$$SimplePictureContainer

At this point, one realized that it is annoying to call the inner
dictionary directly and it would be nicer to expose its methods
on the outside. A simple solution is to leverage on the standard
library class ``UserDict.DictMixin`` which is there just for
that use case. Since we are at it, we can also add the logging
functionality: that means that the low-level interface (calling
directly the inner dictionary methods) will not log whereas
the high level interface will log.

$$PictureContainer

Using ``DictMixin`` is acceptable, since

1.
 ``DictMixin`` provided to its subclasses the standard interface of a
 dictionary, a conceptually tied set of methods;
2.
 the dictionary interface is well know to anybody knowing how
 to use dictionaries in Python, so that the cognitive load is zero;
3.
 ``DictMixin`` allows a substantial code reuse: we redefined explicitly just
 4 methods, but actually we are indirecty affecting 17 other methods:
 ``__cmp__, __contains__, __iter__,
 __len__, __repr__, clear, get, has_key, items, iteritems,
 iterkeys, itervalues, pop, popitem, setdefault, update, values``: without
 ``DictMixin`` we would need to reimplement all of them.

However, notice that in this example the usage of ``DictMixin`` as mixin
class is acceptable, but not optimal: the best solution is to use
``DictMixin`` as a base class, not as a mixin class.

The core problem is that we started from a wrong desing: we wrote
``SimplePictureContainer`` when we did not know of the existence of
``DictMixin``. Now, *a posteriori*, we are trying to fix the mistake by
using multiple inheritance, but that it not the Rigth Thing (TM)
to do. The right thing would be to change the source code of
``SimplePictureContainer`` and to derive directly from ``DictMixin``.

In the real world usually you do not have complete control of the code:
you may leverage on a third party library with a design error, or
simply with an old library, written when ``DictMixin`` did not exist.
In such a situation you may have no way to modify the source code.
Then using ``DictMixin`` and multiple inheritance is a perfectly
acceptable workaround, but it is a workaround still, and it
should not be traded for a clever design.
'''

import logging

from UserDict import DictMixin
import pickle, logging, sys
from datetime import datetime

class SimplePictureContainer(object):

    def __init__(self, id, pictures_or_containers):
      self.id = id
      self.data = {} # the inner dictionary
      for poc in pictures_or_containers: 
        # both pictures and containers must have an .id
        self.data[poc.id] = poc


class PictureContainer(SimplePictureContainer, DictMixin):
  from utility import log

  def __getitem__(self, id):
    return self.data[id]

  def __setitem__(self, id, value):
    self.log.info('Adding or replacing %s into %s', id, self.id)
    self.data[id] = value

  def __delitem__(self, id):
    self.log.warn('Deleting %s', id)
    del self.data[id]

  def keys(self):
    return self.data.keys()

class PictureContainer2(DictMixin):
  from utility import log

  def __init__(self, id, pictures_or_containers):
    self._pc = SimplePictureContainer(id, pictures_or_containers)
    self.data = self._pc.data # avoids an indirection step

  def __getitem__(self, id):
    return self.data[id]

  def __setitem__(self, id, value):
    self.log.info('Adding or replacing %s into %s', id, self.id)
    self.data[id] = value

  def __delitem__(self, id):
    self.log.warn('Deleting %s', id)
    del self.data[id]

  def keys(self):
    return self.data.keys()

  def __getattr__(self, name):
    return getattr(self._pc, name)

# def subclass(cls, base):
#   bases = cls.__bases__
#   if bases and bases != (object,):
#       raise TypeError('Nontrivial base classes %s' % bases)
#   return type(cls.__name__, (base, object), vars(cls).copy())

# class DLPictureContainer(subclass(SimplePictureContainer, DictMixin)):
#   pass

#help(DLPictureContainer)
      
logging.basicConfig(level=logging.INFO, stream=sys.stdout)

class Picture(object):
  def __init__(self, id, location, title, date):
    self.id = id
    self.location = location
    self.title = title
    self.date = date
  def __str__(self):
    return '<%s %s>' % (self.__class__.__name__, self.id)

p1 = Picture('pic00001', '/home/micheles/mypictures/pic00001', 
             "Michele al mare", datetime(2008, 06, 10))

p2 = Picture('pic00002', '/home/micheles/mypictures/pic00002', 
             "Michele in montagna", datetime(2007, 06, 10))

vacanze = PictureContainer("vacanze", [p1, p2])

root = PictureContainer('root', [vacanze])
root['pic00001'] = p1

#     def walk(self):
#       for obj in self.data.itervalues():
#         if not isinstance(obj, self.__class__):
#           yield obj # simple object
#         else: # container object
#           for o in obj.walk():
#             yield o

# for pic in root.walk(): print pic

class WithLog(object):
  "A mixin class"
  @property
  def log(self):
    return logging.getLogger(self.__class__.__name__)

class C(object):
  "A base class"

class C_WithLog(C, WithLog):
  "A mixin-enhanced class"

class CWithLog(C):
  from utility import log # log is a property

if __name__ == '__main__':
  import doctest; doctest.testmod()
