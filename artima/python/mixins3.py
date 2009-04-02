"""
.. _first: http://www.artima.com/weblogs/viewpost.jsp?thread=246341
.. _second: http://www.artima.com/weblogs/viewpost.jsp?thread=246483
.. _multimethod: http://www.artima.com/weblogs/viewpost.jsp?thread=237764
.. _generic function: http://en.wikipedia.org/wiki/Multiple_dispatch
.. _ABC: http://www.python.org/dev/peps/pep-3119/
.. _strait: http://pypi.python.org/pypi/strait

What attitude to keep towards multiple inheritance and mixins
-------------------------------------------------------------------------------

In recent years I have become an opponent of multiple inheritance and
mixins, for reason which I have discussed at length in the
first_ and second_ paper of this series: namespace pollution,
insufficient separation of concerns, fragility with respect to name
clashes, complication of the method resolution order, non scalability
of the design and even simple conceptual confusion with the *is a*
relation.

Moreover, I have argued that mixins are the wrong tool for the job: if
you need a method to be mixed in different classes, then you are
better off not mixing it, putting it at toplevel and promoting it to a
multimethod_/`generic function`_.

Nevertheless, a lot of people think that mixins are very
cool and a lot of new languages present mixins as the panacea
to all evil in object oriented design. This is the reason why
I have started this series, which is intended for developers using
languages which features multiple inheritance or mixins and not
only for Python developers, even if all of my examples here
are in Python, since I am primarily a Python developer.

In my view there are at least three possible attitudes
for a developer using a language with mixins:

1. Resignation. Acknowledge that since the language allows mixins and
   they are used by many frameworks, they will never go away.
   Therefore one should focus on discovering workarounds to cope
   with the situation, like the ``warn_overriding`` decorator that I
   introduced in the first article of this series; one can also write
   better introspection tools to navigate though mixins (the issue
   with pydoc is that it give *too much* information);

2. Education. We (the "experts") should make an effort to communicate
   to the large public the issues with mixins and try to convince framework
   authors to use alternative design. That is what I am trying to accomplish
   with this series.
   
3. Research. Study better implementations of the mixin idea: even if there
   is no hope for the language you are using, research is not useless
   since it may be implemented in languages yet to be written.
   Python itself can be used as an experimentation language, as I show
   in my strait_ module, where I pervert the Python object system to
   become a single inheritance + traits object system, instead of
   a multiple inheritance system. In the fourth paper of this series
   I will show yet another approach, by implementing the mixin idea
   in terms of descriptors and not of inheritance.

Ways to avoid multiple inheritance
------------------------------------------------------------------

While I think that multimethods are the right way to solve the problem
that mixins are tring to solve, the multiple dispatch solution of defining
functions outside classes is a radical departure from the traditional
style of object oriented programming, which is based on single
dispatch and methods defined inside classes.

If you want to keep single dispatch, then there is basically only one
way to avoid inheritance, i.e. to replace it with *composition*,
possibly by adding delegation to the mix.  The practical
implementations of this idea are very different and one can think of
many ways to replace inheritance with composition.

The strait_ module for instance just inject methods in a class
directly (provided they satisfy some checks) and as such it is a not
a big improvements with respect to inheritance: the advantages are in
the protection against name clashes and in the simplication of the
method resolution order. However, one could still argue that those
advantages are not enough, since the namespace pollution
problem is still there.

An alternative solution is to use composition plus delegation, i.e.
to make use of proxies. The cognitive load of a proxy - an object
dispatching to another method - is much smaller than the cognitive
load imposed by inheritance.

If I see an object which is an instance of a class, I feel obliged to
know all the methods of that class, including all the methods of its
ancestors, because I could override them accidentally. On the other
hand, if an object is a proxy, I just note in my mind that it contains
a reference to the proxied object, but I do not feel obliged to know
everything about the proxied object; it is enough for me to know
which methods are called by the proxy methods, if any. It is more
of a psychological effect, but I like proxies since they keep the
complexity confined, whereas inheritance exposes it directly.

In particular, if the proxy has a method which accidentally shadows a
method of the underlying object, nothing particularly bad happens.
That method will not work, but the other methods will keep working,
since they will call the methods of the original object. In
inheritance instead, an accidental overriding may cause havoc, since
other methods of the object may call the accidentally overridden
method, with errors appearing in apparently unrelated portions of
code.

Just to give a concrete example, in the case discussed in the second_
paper of this series, we could have solved the design problem without
using multiple inheritance, just with composition + delegation:

$$PictureContainer

Thanks to the ``__getattr__`` trick, all the methods of
``SimplePictureContainer`` are available to ``PictureContainer2``,
on top of the methods coming from ``DictMixin``: we did basically
fake multiple inheritance without complicating the hierarchy.

A disadvantage of ``PictureContainer2`` is that its instances are no
more instances of ``SimplePictureContainer``, therefore if your code
contained checks like ``isinstance(obj, SimplePictureContainer)``
(which is a very bad practice, at least for Python versions below Python 2.6)
the check would fail. The problem has been solved in 
Python 2.6 thanks to the Abstract Base Class mechanism (ABC_);
it is enought to register ``SimplePictureContainer`` as an ABC of
``PictureContainer2`` and you are done.

An exercise in design
-------------------------------------------------------------

.. image:: box.jpg

Since I do not like to talk in abstract, let me consider a concrete
design problem, which I will solve by using mixin classes but without
incurring in the overpopulation issue. To this aim, let me refer back
to the `second`_ article in this series, specifically to the class
``PictureContainer2`` which inherits from``DictMixin``.
As I said, deriving from ``DictMixin`` is good since ``DictMixin``
provided only 19 attributes (you can see them with ``dir(DictMixin)``)
which are well known to everybody knowing Python dictionaries, therefore
the cognitive load is null.

The problem begins when you decide that you need to add features to
``PictureContainer2``.
For instance, if we are writing a GUI application, we may need methods
such as ``set_thumbnails_size, get_thumbnails_size, 
generate_thumbnails, show_thumbnails, make_picture_menu, show_picture_menu``,
et cetera; let us say we need 50 GUI-related methods or more (methods to set
parameters, methods to make menus, buttons, auxiliary methods and more).
We could include all those methods into a mixin class called 
``GUI`` and we could inherit from both ``DictMixin`` and ``GUI``. 

That's all good. However, suppose version 2.0 of our application is
required to be available through a Web interface; we may therefore need
to implement the 8 methods of the HTTP protocol
(``HEAD, GET, POST, PUT, DELETE, TRACE, OPTIONS, CONNECT``) into another
mixin class. Moreover, if we want to give the ability to edit the images
to our users, we may need to implement a WebDAV interface too, with 7
additional methods ``PROPFIND, PROPPATCH, MKCOL, COPY, MOVE, LOCK, UNLOCK``.

On the other hand, there are users who may prefer the old FTP protocol
to transfer the pictures and therefore we would need to implemente 43
other methods (``ABOR, ALLO, APPE, CDUP, CWD, DELE, EPRT, EPSV, FEAT,
HELP, LIST, MDTM, MLSD, MLST, MODE, MKD, NLST, NOOP, OPTS, PASS, PASV,
PORT, PWD, QUIT, REIN, REST, RETR, RMD, RNFR, RNTO, SIZE, STAT, STOR,
STOU, STRU, SYST, TYPE, USER, XCUP, XCWD, XMKD, XPWD, XRMD``).
Finally, we will need a few authorization-related methods (``is_admin,
is_logged_user, is_anonymous_user, is_picture_owner, is_friend_of``,
eccetera), let's say 20 methods to be stored into another mixin class
``AUTH``.

Now we are left with six mixin classes (``DictMixin``, ``GUI``,
``HTTP``, ``WEBDAV``, ``FTP``, ``AUTH``) and a total of at least 20
(from ``DictMixin``) + 50 (from ``GUI``) + 8 (from ``HTTP``) + 7 (from
``WEBDAV``) + 44 (from ``FTP``) + 20 (from ``AUTH``) = 148 methods
coming from the mixin classes. To those methods you may add the
methods coming from the ``PictureContainer`` class.  This is not nice,
especially if you think that in future versions you may need to
support yet another interface and more mixin methods.  In my estimate
I have been conservative, but it is easy to reach hundreds of
methods. This scenario is exactly what happened in Zope/Plone.

In such a situation one must ask if there are alternative designs that
would avoid the overpopulation problem. The answer is yes, and it is the
standard one: *use composition instead of inheritance*. Everybody recommends
this practice, but yet it is not followed enough in real life.

.. _from Aristotle's times: http://en.wikipedia.org/wiki/Categories_(Aristotle)
.. _articolo precedente: mixins1.html
.. _Plone Site: http://www.phyast.pitt.edu/~micheles/python/plone-hierarchy.png
"""

from UserDict import DictMixin
from mixins2 import Picture, SimplePictureContainer
from datetime import datetime

class PictureContainer(DictMixin):
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
