'''
A possible solution
---------------------------------------------------------------

The design problem described in the last article is clearly a problem
of interfaces. We have an object and we want to interact with it
through multiple interfaces (GUI, HTTP, FTP, etc.). The mixin
solution just adds a bunch of methods for each interface, with the
result of creating a monster object with hundreds of methods.

This is a maintenance nightmare since the human brain can manage a
limited amount of information. An object with ten methods can be
kept in mind easily enough, but an object with a hundred methods is
outside the reach of the average programmer.

The solution is to split the hundred methods into ten
categories with ten methods each: at this point you can keep the ten
categories in your mind.
This solution scales well: if I
needed a thousand methods, I would just define ten macro-categories,
each macro-category including ten micro-categories, and I would keep
in mind the macro-categories.

Hierarchical catalogs are the natural
way to memorize information for the human mind, at least from
`Aristotle's times`_: this is the right solution, not to have
a hundred methods at the same level in the same namespace.

We need therefore a mixin-like solution which however keeps the methods
in separate namespaces *explicitly* (usual mixins keeps the methods
in separate namespaces but *implicitly*, without visibility to the
use of the class).

Technically this idea can be implemented by defining an
*interface wrapper* object
which is also an `attribute descriptor`_:

$$mdispatcher

Interface wrapper objects are proxies: they are intended to wrap the inner
object, by dispatching first on the mixin methods and then to the inner
methods. In practice, if you want to add an interface to an instance ``c``
of a class ``C``, and the methods of the interface are stored into a mixin
class ``M``, you can just add an interface wrapper to ``C``:

.. code-block:: python

  class C(object):
      m = iwrapper(M)

Now ``c.m`` is an instance of ``M`` which can be passed to all
functions and methods expecting the interface defined by ``M`` ;
moreover all the methods and attributes of the underlying object are
available via the ``__getattr__`` trick. For simplicity I
assuming that there are no name clashes between the names of the
interface methods and the names of the inner methods.

Here is how you would use interface wrappers in the ``PictureContainer``
example:

$$PictureContainer

Notice that I have refactored ``PictureContainer`` a bit.  I have
defined the property ``log`` explicitly (before it was imported,
``from utility import log``).  Since it takes only three lines, it
makes sense to write them out and to avoid forcing the reader to look
in another module. There is always a compromise between code reuse and
spaghetti code. When in doubt, I always remind myself that
*readability counts*.

As you see, I have removed all mixin classes except ``DictMixin``.
After all, I have decided that a ``PictureContainer`` *is* like a dictionary,
but it is not really also an object GUI, HTTP, WEBDAV, FTP, AUTH.
Logically those are different interfaces or wrappers over the basic object.

There is still multiple inheritance from ``object``, because
``DictMixin`` is an *old-style* class (for backward compatibility reasons)
whereas interface wrappers, being attribute descriptors,
are intended to be used with *new-style*
classes. Inheriting from ``object`` makes ``PictureContainer`` a new style
class. This is one of the rare cases where multiple inheritance is
convenient, but this use case has already disappeared in Python 3.0,
where all classes are new-style.

.. _Zen di Python: http://www.python.org/dev/peps/pep-0020/

Discussion
-----------------------------------------------------------------

Let me check if the solution to the design problem is consistent
with the `Zen di Python`_.
First of all, the implementation of the interface wrapper concept is simple
- 20 lines of code - and this is already a step in the right direction
(*if the implementation is hard to
explain, it's a bad idea*).
All the methods of the mixin are localized in the mixin namespace
and they do not pollute the namespace of the original class, and
that's good (*namespaces are one honking great idea -- let's do more of
those!*).
Moreover, we are following the  *explicit is better than implicit* principle.
For instance, to access the ``POST`` method of the ``HTTP`` mixin we need 
to write ``self.http.POST``, which is good, since the readers of our code
will not need to guess the origin of the method
(*in the face of ambiguity, refuse the temptation to guess*).

The solution is also usable (*practicality beats purity*): you can instantiate
a ``PictureContainer`` object and perform some experiment from the Python
prompt:

.. code-block:: python

 >>> pc = PictureContainer('root', [])

Autocompletion works pretty well:

.. code-block:: python

 >>> pc.ftp. # press TAB
 pc.ftp.RECV                   
 pc.ftp.SEND       
 ...

the ``help`` function does not provide excessive information

.. code-block:: python

 >>> help(pc)
 Help on PictureContainer in module mixins2 object:
 class PictureContainer(UserDict.DictMixin, __builtin__.object)
 |  Method resolution order:
 |      PictureContainer
 |      UserDict.DictMixin
 |      __builtin__.object
 |  
 |  Methods defined here:
 |  ...
 | 
 |  auth = <AUTHWrapper {is_admin ...} >
 |  ftp = <FTPWrapper {RECV, SEND ...} >
 |  gui = <GUIWrapper {draw_button, draw_buttons ...} >
 |  http = <HTTPWrapper {GET, POST ...} >
 |  webdav = <WEBDAVWrapper {GET, POST, LOCK, UNLOCK ...} >
 ...

and it is possible to introspect just the features you are interested
in, without having everything mixed in (*sparse is better than dense*):

.. code-block:: python

 >>> print dir(pc.http)
 ['GET, 'POST', ...]

It is also easy to adapt an object made from iwrappers.
However to explain this point in detail would require another
series of articles about component programming and adaptation.

Conclusion
----------------------------------------

Is the solution I presented here the only solution or even the best
solution to the problem of adding multiple interfaces to an object?
Certainly not. I have written it down in half an hour an I am not
even using it, since (fortunately) I am not a framework writer.

The solution is intended as a suggestion
for people which are refactoring a framework based
on mixins and which already have they methods organized in mixin
classes. Then, the ``iwrapper`` function is able to convert
such pre-existing classes into objects which can be used as
class attributes, replacing multiple inheritance with composition.

If you do not already have the mixin classes, you may be better
off with a different solution. Moreover, if you are using Python 2.6
or later, it is natural to tackle this problem in terms of
Abstract Base Classes (ABC_), which I have completely ignored.

The solution I have presented lists all the interfaces supported
by an object directly (statically); however you could use a different
design based on adapters, where the object is dynamically adapted
with the correct interface before being passed to its consumer object.
A solution based on adapters is fine if the list of supported
interfaces is not know a priori.

My point here was to show here that Python (at least from Python 2.2)
makes it easy to implement solutions based on composition than than on
inheritance.

Actually, I would say that the general trend of modern Python
frameworks is to favor `component programming`_ rather than
inheritance. You should take in account this fact. Instead of my home
made solution you may want to try out an enterprise-ready solution,
like the component framework of Zope 3 (I personally prefer home made
solutions to over-engineered frameworks, but YMMV).

Nowadays I tend to consider multiple inheritance and mixins more of
a hack than a legitimate design technique: they may be useful when you
need to integrate with pre-existing code with a minimal offert, or as
a debugging tool, when you want to instrument a third party hierarchy,
but you if are designing an application from scratch you are often
better off if you do not rely on mixins.  I usually
recommend to use as little as possible even single inheritance.

.. _ABC: http://www.python.org/dev/peps/pep-3119/
.. _component programming: http://www.muthukadan.net/docs/zca.html
.. _Aristotle's times: http://en.wikipedia.org/wiki/Categories_(Aristotle)
.. _attribute descriptor: http://users.rcn.com/python/download/Descriptor.htm
'''

import os, copy, mdispatcher
from mdispatcher import iwrapper
from UserDict import DictMixin

class GUI(object):
  def draw_button(self, button):
    pass

  def draw_buttons(self, buttons):
    for b in buttons:
      self.draw_button(b)

class HTTP(object):
  def GET(self):
    pass
  def POST(self):
    pass

class FTP(object):
  def RECV(self):
    pass
  def SEND(self):
    pass

class WEBDAV(HTTP):
  def LOCK(self):
    passv
  def UNLOCK(self):
    pass

class AUTH(object):
  def is_admin(self):
    pass

class WebPictureContainer(DictMixin):
    http = iwrapper(HTTP)
    ftp = iwrapper(FTP)
    webdav = iwrapper(WEBDAV)

    def __init__(self, id, pictures_or_containers):
      self.id = id
      for poc in pictures_or_containers:
        self[poc.id] = poc
      
import pickle, logging, sys
from datetime import datetime
logging.basicConfig(level=logging.INFO)

def test():
    c = Container()
    c.http, c.ftp, c.webdav
    print dir(c.http)
    print dir(c.webdav)
    pickle.loads(pickle.dumps(c)).http._obj

class Picture(object):
  def __init__(self, id, location, title, date):
    self.id = id
    self.location = location
    self.title = title
    self.date = date
  def __str__(self):
    return '<%s %s>' % (self.__class__.__name__, self.id)

def walk(container, path='/'):
  for name, obj in container.items():
    newpath = os.path.join(path, name)
    if type(obj) is type(container): # container object
      for n, o in walk(obj, newpath):
        yield n, o
    else: # simple object
      yield newpath, obj

class PictureContainer(DictMixin, object):
  # interface wrappers are instances of the corresponding mixin class;
  # moreover they dispatch on self, i.e. on PictureContainer objects
  gui = iwrapper(GUI)
  http = iwrapper(HTTP)
  webdav = iwrapper(WEBDAV)
  ftp = iwrapper(FTP)
  auth = iwrapper(AUTH)

  @property
  def log(self):
    return logging.getLogger(self.__class__.__name__)

  def __init__(self, id, pictures_or_containers):
      self.id = id
      self.data = {}
      for poc in pictures_or_containers: 
        # both pictures and containers must have an .id
        self.data[poc.id] = poc

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

def makecontainer():
  p1 = Picture('pic00001', '/home/micheles/mypictures/pic00001', 
              "Michele al mare", datetime(2008, 06, 10))

  p2 = Picture('pic00002', '/home/micheles/mypictures/pic00002', 
              "Michele in montagna", datetime(2007, 06, 10))

  vacanze = PictureContainer("vacanze", [p1, p2])

  root = PictureContainer('root', [vacanze])
  root['pic00001'] = p1
  return root

pc = PictureContainer('root', [])

gui = pc.gui

#help(pc)
#print dir(root.http)


if __name__ == '__main__': # test
    class M(object):
        def helper(self):
            return 1
        def add1(self, x):
            return self.helper() + x
    class C(object):
        m = iwrapper(M)
    c = C()
    print c.m.add1(2)
    print c.m
