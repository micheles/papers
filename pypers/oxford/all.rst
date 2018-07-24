Lectures on Advanced Python Programming 
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. image:: accu2005.png

:Author: Michele Simionato 
:Given: 19 April 2005
:Revised: 7 September 2005

.. contents::



Lecture 1: Loops (i.e. iterators & generators)
==============================================

Part I: iterators
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Iterators are everywhere
--------------------------------

>>> for i in 1, 2, 3:
...     print i
1
2
3

The 'for' loop is using *iterators* internally::

 it = iter((1,2,3))
 while True:
     try:
         print it.next()
     except StopIteration:
         break

Iterables and iterators
--------------------------

*Iterable* = anything you can loop over = any sequence + any object with an __iter__ method;

Not every sequence has an __iter__ method:

>>> "hello".__iter__()
Traceback (most recent call last):
  ...
AttributeError: 'str' object has no attribute '__iter__'

*Iterator* = any object with a .next method and an __iter__ method returning self

Simpler way to get an iterator
--------------------------------------------------------

>>> it = iter("hello")
>>> it.next()
'h'
>>> it.next()
'e'
>>> it.next()
'l'
>>> it.next()
'l'
>>> it.next()
'o'
>>> it.next()
Traceback (most recent call last):
  ...
StopIteration

Sentinel syntax iter(callable, sentinel)
--------------------------------------------

Example::

 $ echo -e "value1\nvalue2\nEND\n" > data.txt
 $ python -c "print list(iter(file('data.txt').readline, 'END\n'))"
 ['value1\n', 'value2\n']

Beware of infinite iterators:

>>> repeat = iter(lambda : "some value", "")
>>> repeat.next()
'some value'

Second simpler way to get an iterator: generator-expressions
-------------------------------------------------------------

>>> squares = (i*i for i in range(1,11)) 
>>> list(squares)
[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

Excessive parenthesis can be skipped, so use

>>> dict((i, i*i) for i in range(1,11))
{1: 1, 2: 4, 3: 9, 4: 16, 5: 25, 6: 36, 7: 49, 8: 64, 9: 81, 10: 100}

instead of

>>> dict([(i, i*i) for i in range(1,11)])
{1: 1, 2: 4, 3: 9, 4: 16, 5: 25, 6: 36, 7: 49, 8: 64, 9: 81, 10: 100}

(as usual, the most elegant version is the most efficient).

Iteration caveats
--------------------------

>>> ls = [i for i in (1,2,3)]
>>> i 
3

>>> it = (j for j in (1,2,3))
>>> j 
Traceback (most recent call last):
  ...
NameError: name 'j' is not defined

A subtler example:

>>> ls = [lambda :i for i in (1,2,3)]
>>> ls[0]()
3

instead

>>> it = (lambda :i for i in (1,2,3))
>>> it.next()()
1
>>> it.next()()
2
>>> it.next()()
3

*seems* to be working but it is not really the case:

>>> it = (lambda :i for i in (1,2,3))
>>> f1 = it.next()
>>> f2 = it.next()
>>> f3 = it.next()
>>> f1()
3

The reason is that Python does LATE binding *always*. The solution is ugly:

>>> it = list(lambda i=i:i for i in (1,2,3))
>>> it[0]()
1
>>> it[1]()
2
>>> it[2]()
3

Part II: generators 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Trivial example:

>>> def gen123(): # "function" which returns an iterator over the values 1, 2, 3
...     yield 1
...     yield 2
...     yield 3
...
>>> it = gen123()
>>> it.next()
1
>>> it.next()
2
>>> it.next()
3
>>> it.next()
Traceback (most recent call last):
  ...
StopIteration

Real life example: using generators to generate HTML tables

::

   #<htmltable.py>

   def HTMLTablegen(table):
       yield "<table>"
       for row in table:
          yield "<tr>"
          for col in row:
              yield "<td>%s</td>" % col
          yield "</tr>"
       yield "</table>"

   def test():
       return "\n".join(HTMLTablegen([["Row", "City"], 
                          [1,'London'], [2, 'Oxford']]))

   if __name__ == "__main__": # example
       print test()

   #</htmltable.py>

>>> from htmltable import test
>>> print test()
<table>
<tr>
<td>Row</td>
<td>City</td>
</tr>
<tr>
<td>1</td>
<td>London</td>
</tr>
<tr>
<td>2</td>
<td>Oxford</td>
</tr>
</table>

A simple recipe: skip redundant
---------------------------------

How to remove duplicates by keeping the order::

 #<skip_redundant.py>

 def skip_redundant(iterable, skipset=None):
    "Redundant items are repeated items or items in the original skipset."
    if skipset is None: skipset = set()
    for item in iterable:
        if item not in skipset:
            skipset.add(item)
            yield item
          
 #</skip_redundant.py>

>>> from skip_redundant import skip_redundant
>>> print list(skip_redundant("<hello, world>", skipset=set("<>")))
['h', 'e', 'l', 'o', ',', ' ', 'w', 'r', 'd']

Another real life example: working with nested structures
----------------------------------------------------------

::

 #<walk.py>

 def walk(iterable, level=0):
     for obj in iterable:
         if not hasattr(obj, "__iter__"): # atomic object
             yield obj, level
         else: # composed object: iterate again
             for subobj, lvl in walk(obj, level + 1):
                 yield subobj, lvl

 def flatten(iterable):
     return (obj for obj, level in walk(iterable))
        
 def pprint(iterable):
     for obj, level in walk(iterable):
         print " "*level, obj
        
 #</walk.py>

>>> from walk import flatten, pprint
>>> nested_ls = [1,[2,[3,[[[4,5],6]]]],7]
>>> pprint(nested_ls)     
 1
  2
   3
      4
      5
     6
 7
>>> pprint(flatten(nested_ls))
 1
 2
 3
 4
 5
 6
 7

Another typical use case for generators: parsers
---------------------------------------------------------

A very stripped down parser for nested expressions

::

 #<sexpr2indent.py>
 """A simple s-expression formatter."""

 import re

 def parse(sexpr):
     position = 0
     nesting_level = 0
     paren = re.compile(r"(?P<paren_beg>\()|(?P<paren_end>\))")
     while True:
         match = paren.search(sexpr, position)
         if match:
             yield nesting_level, sexpr[position: match.start()]
             if match.lastgroup == "paren_beg":
                 nesting_level += 1
             elif match.lastgroup == "paren_end":
                 nesting_level -= 1
             position = match.end()
         else:
             break

 def sexpr_indent(sexpr):
     for nesting, text in parse(sexpr.replace("\n", "")):
         if text.strip():  print " "*nesting, text

 #</sexpr2indent.py>

>>> from sexpr2indent import sexpr_indent
>>> sexpr_indent("""\
... (html (head (title Example)) (body (h1 s-expr formatter example)
... (a (@ (href http://www.example.com)) A link)))""")
... #doctest: +NORMALIZE_WHITESPACE
  html
   head
    title Example
    body
    h1 s-expr formatter example
    a
     @
      href http://www.example.com
     A link


Other kinds of iterables
------------------------------------------------

The following class generates iterable which are not iterators:
::

 #<reiterable.py>

 class ReIter(object):
     "A re-iterable object."
     def __iter__(self):
         yield 1
         yield 2
         yield 3

 #</reiterable.py>

>>> from reiterable import ReIter
>>> rit = ReIter()
>>> list(rit)
[1, 2, 3]
>>> list(rit) # it is reiterable!
[1, 2, 3]

The itertools module
----------------------------------------------------

  - count([n]) --> n, n+1, n+2, ...
  - cycle(p) --> p0, p1, ... plast, p0, p1, ...
  - repeat(elem [,n]) --> elem, elem, elem, ... endlessly or up to n times
  - izip(p, q, ...) --> (p[0], q[0]), (p[1], q[1]), ...
  - ifilter(pred, seq) --> elements of seq where pred(elem) is True
  - ifilterfalse(pred, seq) --> elements of seq where pred(elem) is False
  - islice(seq, [start,] stop [, step]) --> elements from seq[start:stop:step]
  - imap(fun, p, q, ...) --> fun(p0, q0), fun(p1, q1), ...
  - starmap(fun, seq) --> fun(\*seq[0]), fun(\*seq[1]), ...
  - tee(it, n=2) --> (it1, it2 , ... itn) splits one iterator into n
  - chain(p, q, ...) --> p0, p1, ... plast, q0, q1, ...
  - takewhile(pred, seq) --> seq[0], seq[1], until pred fails
  - dropwhile(pred, seq) --> seq[n], seq[n+1], starting when pred fails
  - groupby(iterable[, keyfunc]) --> sub-iterators grouped by value of keyfunc(v)

anyTrue
------------------------------

>>> import itertools
>>> def anyTrue(predicate, iterable): 
...     return True in itertools.imap(predicate, iterable)
...
>>> fname = "picture.gif"
>>> anyTrue(fname.endswith, ".jpg .gif .png".split())
True

AnyTrue does *short-circuit*:

>>> def is3(i):
...     print "i=%s" % i
...     return i == 3

>>> anyTrue(is3, range(10))
i=0
i=1
i=2
i=3
True

chop
----------------------

You want to chop an iterable in batches of a given size:

>>> from chop import chop
>>> list(chop([1, 2, 3, 4], 2))
[[1, 2], [3, 4]]
>>> list(chop([1, 2, 3, 4, 5, 6, 7],3))
[[1, 2, 3], [4, 5, 6], [7]]

Here is a possible implementation::

 #<chop.py>

 # see also http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/303279

 import itertools

 def chop(iterable, batchsize):
     it = iter(iterable)
     while True:
         batch = list(itertools.islice(it, batchsize))
         if batch: yield batch
         else: break

 #</chop.py>

For people thinking Python is too readable, here is a one-liner:

>>> chop = lambda it, n : itertools.izip(*(iter(it),)*n)
...
>>> list(chop([1,2,3,4], 2))
[(1, 2), (3, 4)]

tee
-----------------------

To make copies of iterables; typically used in parsers:

>>> from itertools import tee, chain, izip
>>> chars, prevs = tee("abc")
>>> prevs = chain([None], prevs)
>>> for char, prev in izip(chars, prevs):
...     print char, prev
...
a None
b a
c b

Grouping and sorting
----------------------

>>> from itertools import groupby
>>> from operator import itemgetter

>>> NAME, AGE = 0, 1
>>> query_result = ("Smith", 34), ("Donaldson", 34), ("Lee", 22), ("Orr", 22)

Grouping together people of the same age:

>>> for k, g in groupby(query_result, key=itemgetter(AGE)):
...     print k, list(g)
...
34 [('Smith', 34), ('Donaldson', 34)]
22 [('Lee', 22), ('Orr', 22)]

Sorting by name:

>>> for tup in sorted(query_result, key=itemgetter(NAME)):
...     print tup
('Donaldson', 34)
('Lee', 22)
('Orr', 22)
('Smith', 34)



Lecture 2: Objects (delegation & inheritance)
==============================================

Part I: delegation
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Understanding how attribute access works: internal delegation via *descriptors*

Accessing simple attributes
--------------------------------

>>> class C(object):
...     a = 2
...     def __init__(self, x):
...        self.x = x
...

>>> c = C(1)
>>> c.x
1
>>> c.a
2

We are retrieving

>>> c.__dict__["x"]
1

If there is nothing in c.__dict__, Python looks at C.__dict__:

>>> print c.__dict__.get("a")
None

>>> C.__dict__["a"]
2

If there is nothing in C.__dict__, Python looks at the superclasses according
to the MRO (see part II).

Accessing methods
--------------------------------------------------------

>>> c.__init__ #doctest: +ELLIPSIS
<bound method C.__init__ of <__main__.C object at 0x...>>

since __init__ is not in c.__dict__ Python looks in the class dictionary 
and finds

>>> C.__dict__["__init__"] #doctest: +ELLIPSIS
<function __init__ at 0x...>

Then it magically converts the function into a method bound to the instance
"c". 

NOTE: this mechanism works for new-style classes only.

The old-style mechanism is less consistent and the attribute lookup of special
methods is special: (*)

>>> class C(object): pass
>>> c = C()
>>> c.__str__ = lambda : "hello!"
>>> print c #doctest: +ELLIPSIS
<__main__.C object at ...>

whereas for old-style classes

>>> class C: pass
>>> c = C()
>>> c.__str__ = lambda : "hello!"
>>> print c 
hello!

the special method is looked for in the instance dictionary too.

(*) modulo a very subtle difference for __getattr__-delegated special methods,
see later.

Converting functions into methods
-------------------------------------

It is possible to convert a function into a bound or unbound method
by invoking the ``__get__`` special method:

>>> def f(x): pass
>>> f.__get__ #doctest: +ELLIPSIS
<method-wrapper object at 0x...>

>>> class C(object): pass
...

>>> def f(self): pass
...
>>> f.__get__(C(), C) #doctest: +ELLIPSIS
<bound method C.f of <__main__.C object at 0x...>>

>>> f.__get__(None, C)
<unbound method C.f>

Functions are the simplest example of *descriptors*.

Access to methods works since internally Python transforms 

     ``c.__init__ -> type(c).__dict__['__init__'].__get__(c, type(c))``


Note: not *all* functions are descriptors:

>>> from operator import add
>>> add.__get__
Traceback (most recent call last):
  ...
AttributeError: 'builtin_function_or_method' object has no attribute '__get__'

Hack: a very slick adder
-----------------------------

The descriptor protocol can be (ab)used as a way to avoid the late binding
issue in for loops:

>>> def add(x,y):
...     return x + y
>>> closures = [add.__get__(i) for i in range(10)]
>>> closures[5](1000)
1005

Notice: operator.add will not work.

Descriptor Protocol
----------------------

Everything at http://users.rcn.com/python/download/Descriptor.htm

Formally::

 descr.__get__(self, obj, type=None) --> value
 descr.__set__(self, obj, value) --> None
 descr.__delete__(self, obj) --> None

Examples of custom descriptors::

 #<descriptor.py>


 class AttributeDescriptor(object):
    def __get__(self, obj, cls=None):
        if obj is None and cls is None:
            raise TypeError("__get__(None, None) is invalid")
        elif obj is None:
            return self.get_from_class(cls)
        else:
            return self.get_from_obj(obj)
    def get_from_class(self, cls):
        print "Getting %s from %s" % (self, cls)
    def get_from_obj(self, obj):
        print "Getting %s from %s" % (self, obj)


 class Staticmethod(AttributeDescriptor):
    def __init__(self, func):
        self.func = func
    def get_from_class(self, cls):
        return self.func
    get_from_obj = get_from_class


 class Classmethod(AttributeDescriptor):
    def __init__(self, func):
        self.func = func
    def get_from_class(self, cls):
        return self.func.__get__(cls, type(cls))
    def get_from_obj(self, obj):
        return self.get_from_class(obj.__class__)

 class C(object):
    s = Staticmethod(lambda : 1)
    c = Classmethod(lambda cls : cls.__name__)

 c = C()

 assert C.s() == c.s() == 1
 assert C.c() == c.c() == "C"

 #</descriptor.py>

Multilingual attribute
----------------------

Inspirated by a question in the Italian Newsgroup::

 #<multilingual.py>

 import sys
 from descriptor import AttributeDescriptor

 class MultilingualAttribute(AttributeDescriptor):
     """When a MultilingualAttribute is accessed, you get the translation 
     corresponding to the currently selected language.
     """
     def __init__(self, **translations):
         self.trans = translations
     def get_from_class(self, cls):
         return self.trans[getattr(cls, "language", None) or
                          sys.modules[cls.__module__].language]
     def get_from_obj(self, obj):
         return self.trans[getattr(obj, "language", None) or
                          sys.modules[obj.__class__.__module__].language]
      

 language = "en"
 
 # a dummy User class
 class DefaultUser(object):
     def has_permission(self):
         return False
    
 class WebApplication(object):
     error_msg = MultilingualAttribute(
         en="You cannot access this page",
         it="Questa pagina non e' accessibile",
         fr="Vous ne pouvez pas acceder cette page",)
     user = DefaultUser()
     def __init__(self, language=None):
         self.language = language or getattr(self.__class__, "language", None)
     def show_page(self):
         if not self.user.has_permission():
             return self.error_msg


 app = WebApplication()
 assert app.show_page() == "You cannot access this page"

 app.language = "fr"
 assert app.show_page() == "Vous ne pouvez pas acceder cette page"

 app.language = "it"
 assert app.show_page() == "Questa pagina non e' accessibile"

 app.language = "en"
 assert app.show_page() == "You cannot access this page"

 #</multilingual.py>

The same can be done with properties::

 #<multilingualprop.py>

 language = "en"

 # a dummy User class
 class DefaultUser(object):
     def has_permission(self):
         return False
    
 def multilingualProperty(**trans):
     def get(self):
         return trans[self.language]
     def set(self, value):
         trans[self.language] = value 
     return property(get, set)

 class WebApplication(object):
     language = language
     error_msg = multilingualProperty(
         en="You cannot access this page",
         it="Questa pagina non e' accessibile",
         fr="Vous ne pouvez pas acceder cette page",)
     user = DefaultUser()
     def __init__(self, language=None):
         if language: self.language = self.language
     def show_page(self):
         if not self.user.has_permission():
             return self.error_msg
 
 #</multilingualprop.py>

This also gives the possibility to set the error messages.

The difference with the descriptor approach

>>> from multilingual import WebApplication
>>> app = WebApplication()
>>> print app.error_msg
You cannot access this page
>>> print WebApplication.error_msg
You cannot access this page

is that with properties there is no nice access from the class:

>>> from multilingualprop import WebApplication
>>> WebApplication.error_msg #doctest: +ELLIPSIS
<property object at ...>

Another use case for properties: storing users
------------------------------------------------------------

Consider a library providing a simple User class::

 #<crypt_user.py>

 class User(object):
     def __init__(self, username, password):
         self.username, self.password = username, password

 #</crypt_user.py>

The User objects are stored in a database as they are.
For security purpose, in a second version of the library it is
decided to crypt the password, so that only crypted passwords
are stored in the database. With properties, it is possible to
implement this functionality without changing the source code for 
the User class::

 #<crypt_user.py>

 from crypt import crypt

 def cryptedAttribute(seed="x"):
     def get(self):
         return getattr(self, "_pw", None)
     def set(self, value):
         self._pw = crypt(value, seed)
     return property(get, set)
    
 User.password = cryptedAttribute()

#</crypt_user.py>

>>> from crypt_user import User
>>> u = User("michele", "secret")
>>> print u.password
xxZREZpkHZpkI

Notice the property factory approach used here.

Low-level delegation via __getattribute__
------------------------------------------------------------------

Attribute access is managed by the__getattribute__ special method::

 #<tracedaccess.py>

 class TracedAccess(object):
     def __getattribute__(self, name):
         print "Accessing %s" % name
         return object.__getattribute__(self, name)


 class C(TracedAccess):
     s = staticmethod(lambda : 'staticmethod')
     c = classmethod(lambda cls: 'classmethod')
     m = lambda self: 'method'
     a = "hello"

 #</tracedaccess.py>

>>> from tracedaccess import C
>>> c = C()
>>> print c.s()
Accessing s
staticmethod
>>> print c.c()
Accessing c
classmethod
>>> print c.m()
Accessing m
method
>>> print c.a
Accessing a
hello
>>> print c.__init__ #doctest: +ELLIPSIS
Accessing __init__
<method-wrapper object at 0x...>
>>> try: c.x
... except AttributeError, e: print e
...
Accessing x
'C' object has no attribute 'x'

>>> c.y = 'y'
>>> c.y
Accessing y
'y'

You are probably familiar with ``__getattr__`` which is similar 
to ``__getattribute__``, but it is called *only for missing attributes*.

Traditional delegation via __getattr__
--------------------------------------------------------

Realistic use case in "object publishing"::

 #<webapp.py>

 class WebApplication(object):
     def __getattr__(self, name):
         return name.capitalize()


 app = WebApplication()

 assert app.page1 == 'Page1'
 assert app.page2 == 'Page2'

 #</webapp.py>

Here is another use case in HTML generation::

 #<XMLtag.py>

 def makeattr(dict_or_list_of_pairs):
     dic = dict(dict_or_list_of_pairs) 
     return " ".join('%s="%s"' % (k, dic[k]) for k in dic) # simplistic

 class XMLTag(object):
     def __getattr__(self, name):
         def tag(value, **attr):
             """value can be a string or a sequence of strings."""
             if hasattr(value, "__iter__"): # is iterable
                 value = " ".join(value)
             return "<%s %s>%s</%s>" % (name, makeattr(attr), value, name)
         return tag

 class XMLShortTag(object):
     def __getattr__(self, name):
         def tag(**attr):
             return "<%s %s />" % (name, makeattr(attr))
         return tag

 tag = XMLTag()
 tg = XMLShortTag()

 #</XMLtag.py>

>>> from XMLtag import tag, tg
>>> print tag.a("example.com", href="http://www.example.com")
<a href="http://www.example.com">example.com</a>
>>> print tg.br(**{'class':"br_style"})
<br class="br_style" />

Keyword dictionaries with __getattr__/__setattr__
---------------------------------------------------
::

 #<kwdict.py>

 class kwdict(dict): # or UserDict, to make it to work with Zope
     """A typing shortcut used in place of a keyword dictionary."""
     def __getattr__(self, name):
         return self[name]
     def __setattr__(self, name, value):
         self[name] = value

 #</kwdict.py>

And now for a completely different solution::

 #<dictwrapper.py>

 class DictWrapper(object): 
     def __init__(self, **kw):
         self.__dict__.update(kw)

 #</dictwrapper.py>


Delegation to special methods caveat
--------------------------------------

>>> class ListWrapper(object):
...     def __init__(self, ls):
...         self._list = ls
...     def __getattr__(self, name):
...         if name == "__getitem__": # special method
...             return self._list.__getitem__
...         elif name == "reverse": # regular method
...             return self._list.reverse
...         else:
...             raise AttributeError("%r is not defined" % name)
... 
>>> lw = ListWrapper([0,1,2])
>>> print lw.x
Traceback (most recent call last):
  ...
AttributeError: 'x' is not defined

>>> lw.reverse()
>>> print lw.__getitem__(0)
2
>>> print lw.__getitem__(1)
1
>>> print lw.__getitem__(2)
0
>>> print lw[0]
Traceback (most recent call last):
  ...
TypeError: unindexable object


Part II: Inheritance 
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

The major changes in inheritance from Python 2.1 to 2.2+ are:

1. you can subclass built-in types (as a consequence the constructor__new__ 
   has been exposed to the user, to help subclassing immutable types);
2. the Method Resolution Order (MRO) has changed;
3. now Python allows *cooperative method calls*, i.e. we have *super*.

Why you need to know about MI even if you do not use it
-----------------------------------------------------------

In principle, the last two changes are relevant only if you use multiple 
inheritance. If you use single inheritance only, you don't need ``super``:
you can just name the superclass.
However, somebody else may want to use your class in a MI hierarchy,
and you would make her life difficult if you don't use ``super``.

My SI hierarchy::

 #<why_super.py>

 class Base(object):
     def __init__(self):
         print "B.__init__"

 class MyClass(Base):
     "I do not cooperate with others"
     def __init__(self):
         print "MyClass.__init__"
         Base.__init__(self)  #instead of super(MyClass, self).__init__()

 #</why_super.py>

Her MI hierarchy::

 #<why_super.py>

 class Mixin(Base):
     "I am cooperative with others"
     def __init__(self):
         print "Mixin.__init__"
         super(Mixin, self).__init__()

 class HerClass(MyClass, Mixin):
     "I am supposed to be cooperative too"
     def __init__(self):
         print "HerClass.__init__"
         super(HerClass, self).__init__()

 #</why_super.py>

>>> from why_super import HerClass
>>> h = HerClass() # Mixin.__init__ is not called!
HerClass.__init__
MyClass.__init__
B.__init__

 ::

                  4 object  
                      |
                   3 Base
                  /      \
            1 MyClass  2 Mixin
                   \     /
                 0 HerClass

>>> [ancestor.__name__ for ancestor in HerClass.mro()]
['HerClass', 'MyClass', 'Mixin', 'Base', 'object']

In order to be polite versus your future users, you should use ``super`` 
always. This adds a cognitive burden even for people not using MI :-(

Notice that there is no good comprehensive reference on ``super`` (yet)
Your best bet is still http://www.python.org/2.2.3/descrintro.html#cooperation

The MRO instead is explained here: http://www.python.org/2.3/mro.html

Notice that I DO NOT recommand Multiple Inheritance.

More often than not you are better off using composition/delegation/wrapping, 
etc.

See Zope 2 -> Zope 3 experience.

A few details about ``super`` (not the whole truth)
------------------------------------------------------------------------------

>>> class B(object):
...     def __init__(self): print "B.__init__"
...
>>> class C(B):
...     def __init__(self): print "C.__init__"
...
>>> c = C()
C.__init__

``super(cls, instance)``, where ``instance`` is an instance of ``cls`` or of
a subclass of ``cls``, retrieves the right method in the MRO:

>>> super(C, c).__init__ #doctest: +ELLIPSIS
<bound method C.__init__ of <__main__.C object at 0x...>>

>>> super(C, c).__init__.im_func is B.__init__.im_func
True

>>> super(C, c).__init__()
B.__init__

``super(cls, subclass)`` works for unbound methods:

>>> super(C, C).__init__
<unbound method C.__init__>

>>> super(C, C).__init__.im_func is B.__init__.im_func
True
>>> super(C, C).__init__(c)
B.__init__

``super(cls, subclass)`` is also necessary for classmethods and staticmethods. 
Properties and custom descriptorsw works too::

 #<super_ex.py>

 from descriptor import AttributeDescriptor

 class B(object):
    @staticmethod
    def sm(): return "staticmethod"

    @classmethod
    def cm(cls): return cls.__name__

    p = property()
    a = AttributeDescriptor()

 class C(B): pass

 #</super_ex.py>

>>> from super_ex import C

Staticmethod usage:

>>> super(C, C).sm #doctest: +ELLIPSIS
<function sm at 0x...>
>>> super(C, C).sm()
'staticmethod'

Classmethod usage:

>>> super(C, C()).cm
<bound method type.cm of <class 'super_ex.C'>>
>>> super(C, C).cm() # C is automatically passed
'C'

Property usage:

>>> print super(C, C).p #doctest: +ELLIPSIS
<property object at 0x...>
>>> super(C, C).a #doctest: +ELLIPSIS
Getting <descriptor.AttributeDescriptor object at 0x...> from <class 'super_ex.C'>

``super`` does not work with old-style classes, however you can use the
following trick::

 #<super_old_new.py>
 class O:
     def __init__(self):
         print "O.__init__"

 class N(O, object):
     def __init__(self):
         print "N.__init__"
         super(N, self).__init__()

 #</super_old_new.py>

>>> from super_old_new import N
>>> new = N()
N.__init__
O.__init__

There are dozens of tricky points concerning ``super``, be warned!

Subclassing built-in types; __new__ vs. __init__
-----------------------------------------------------

::

 #<point.py>

 class NotWorkingPoint(tuple):
     def __init__(self, x, y):
         super(NotWorkingPoint, self).__init__((x,y))
         self.x, self.y = x, y

 #</point.py>

>>> from point import NotWorkingPoint
>>> p = NotWorkingPoint(2,3)
Traceback (most recent call last):
  ...
TypeError: tuple() takes at most 1 argument (2 given)

::

 #<point.py>

 class Point(tuple):
     def __new__(cls, x, y):
         return super(Point, cls).__new__(cls, (x,y))
     def __init__(self, x, y):
         super(Point, self).__init__((x, y))
         self.x, self.y = x, y

 #</point.py>

Notice that__new__ is a staticmethod, not a classmethod, so one needs
to pass the class explicitely.

>>> from point import Point
>>> p = Point(2,3)
>>> print p, p.x, p.y
(2, 3) 2 3

Be careful when using __new__ with mutable types
------------------------------------------------

>>> class ListWithDefault(list):
...     def __new__(cls):
...         return super(ListWithDefault, cls).__new__(cls, ["hello"])
...
>>> print ListWithDefault() # beware! NOT ["hello"]!
[]

Reason: lists are re-initialized to empty lists in list.__init__!

Instead

>>> class ListWithDefault(list):
...     def __init__(self):
...         super(ListWithDefault, self).__init__(["hello"])
...
>>> print ListWithDefault() # works!
['hello']



Lecture 3: Magic (i.e. decorators and metaclasses)
================================================================

Part I: decorators
+++++++++++++++++++++++++++++++++++++++++++++++++++

Decorators are just sugar: their functionality was already in the language

>>> def s(): pass
>>> s = staticmethod(s)

>>> @staticmethod
... def s(): pass
...

However sugar *does* matter.

A typical decorator: traced
-----------------------------
::

 #<traced.py>

 def traced(func):
     def tracedfunc(*args, **kw):
         print "calling %s.%s" % (func.__module__, func.__name__)
         return func(*args, **kw)
     tracedfunc.__name__ = func.__name__
     return tracedfunc

 @traced
 def f(): pass

 #</traced.py>

>>> from traced import f
>>> f()
calling traced.f

A decorator factory: Timed
------------------------------------------

::

 #<timed.py>

 import sys, time

 class Timed(object):
     """Decorator factory: each decorator object wraps a function and 
     executes it many times (default 100 times).
     The average time spent in one iteration, expressed in milliseconds, 
     is stored in the attributes wrappedfunc.time and wrappedfunc.clocktime,
     and displayed into a log file which defaults to stdout.
     """
     def __init__(self, repeat=100, logfile=sys.stdout):
         self.repeat = repeat
         self.logfile = logfile
     def __call__(self, func):
         def wrappedfunc(*args, **kw):
             fullname = "%s.%s ..." % (func.__module__, func.func_name)
             print >> self.logfile, 'Executing %s' % fullname.ljust(30),
             time1 = time.time()
             clocktime1 = time.clock()
             for i in xrange(self.repeat):
                 res = func(*args,**kw) # executes func self.repeat times
             time2 = time.time()
             clocktime2 = time.clock()
             wrappedfunc.time = 1000*(time2-time1)/self.repeat
             wrappedfunc.clocktime = 1000*(clocktime2 - clocktime1)/self.repeat
             print >> self.logfile, \
                   'Real time: %s ms;' % self.rounding(wrappedfunc.time),
             print >> self.logfile, \
                   'Clock time: %s ms' % self.rounding(wrappedfunc.clocktime)
             return res
         wrappedfunc.func_name = func.func_name
         wrappedfunc.__module__ = func.__module__
         return wrappedfunc
     @staticmethod
     def rounding(float_):
         "Three digits rounding for small numbers, 1 digit rounding otherwise."
         if float_ < 10.:
             return "%5.3f" % float_
         else:
             return "%5.1f" % float_
    
 #</timed.py>

>>> from timed import Timed
>>> from random import sample
>>> example_ls = sample(xrange(1000000), 1000)
>>> @Timed()
... def list_sort(ls):
...     ls.sort()
... 
>>> list_sort(example_ls) #doctest: +ELLIPSIS
Executing __main__.list_sort ... Real time: 0... ms; Clock time: 0... ms


A powerful decorator pattern
--------------------------------
::

 #<traced_function2.py>

 from decorators import decorator

 def trace(f, *args, **kw):
     print "calling %s with args %s, %s" % (f.func_name, args, kw) 
     return f(*args, **kw)

 traced_function = decorator(trace)

 @traced_function
 def f1(x):
     pass

 @traced_function
 def f2(x, y):
     pass

 #</traced_function2.py>

>>> from traced_function2 import traced_function, f1, f2
>>> f1(1)
calling f1 with args (1,), {}
>>> f2(1,2)
calling f2 with args (1, 2), {}

works with pydoc::

 $ pydoc2.4 traced_function2.f2  
 Help on function f1 in traced_function2:

 traced_function2.f1 = f1(x)

 $ pydoc2.4 traced_function2.f2  
 Help on function f2 in traced_function2:

 traced_function2.f2 = f2(x, y)

Here is the source code::

 #<decorators.py>
 
 import inspect, itertools

 def getinfo(func):
     """Return an info dictionary containing:
     - name (the name of the function : str)
     - argnames (the names of the arguments : list)
     - defarg (the values of the default arguments : list)
     - fullsign (the full signature : str)
     - shortsign (the short signature : str)
     - arg0 ... argn (shortcuts for the names of the arguments)
 
     >> def f(self, x=1, y=2, *args, **kw): pass
 
     >> info = getinfo(f)
 
     >> info["name"]
     'f'
     >> info["argnames"]
     ['self', 'x', 'y', 'args', 'kw']
     
     >> info["defarg"]
     (1, 2)
 
     >> info["shortsign"]
     'self, x, y, *args, **kw'
     
     >> info["fullsign"]
     'self, x=defarg[0], y=defarg[1], *args, **kw'
 
     >> info["arg0"], info["arg1"], info["arg2"], info["arg3"], info["arg4"]
     ('self', 'x', 'y', 'args', 'kw')
     """
     assert inspect.ismethod(func) or inspect.isfunction(func)
     regargs, varargs, varkwargs, defaults = inspect.getargspec(func)
     argnames = list(regargs)
     if varargs: argnames.append(varargs)
     if varkwargs: argnames.append(varkwargs)
     counter = itertools.count()
     fullsign = inspect.formatargspec(
         regargs, varargs, varkwargs, defaults,
         formatvalue=lambda value: "=defarg[%i]" % counter.next())[1:-1]
     shortsign = inspect.formatargspec(
         regargs, varargs, varkwargs, defaults,
         formatvalue=lambda value: "")[1:-1]
     dic = dict(("arg%s" % n, name) for n, name in enumerate(argnames))
     dic.update(name=func.__name__, argnames=argnames, shortsign=shortsign,
         fullsign = fullsign, defarg = func.func_defaults or ())
     return dic
 
 def _contains_reserved_names(dic): # helper
     return "_call_" in dic or "_func_" in dic
 
 def _decorate(func, caller):
     """Takes a function and a caller and returns the function
     decorated with that caller. The decorated function is obtained
     by evaluating a lambda function with the correct signature.
     """
     infodict = getinfo(func)
     assert not _contains_reserved_names(infodict["argnames"]), \
            "You cannot use _call_ or _func_ as argument names!"
     execdict=dict(_func_=func, _call_=caller, defarg=func.func_defaults or ())
     if func.__name__ == "<lambda>":
         lambda_src = "lambda %(fullsign)s: _call_(_func_, %(shortsign)s)" \
                      % infodict
         dec_func = eval(lambda_src, execdict)
     else:
         func_src = """def %(name)s(%(fullsign)s):
         return _call_(_func_, %(shortsign)s)""" % infodict
         exec func_src in execdict 
         dec_func = execdict[func.__name__]
     dec_func.__doc__ = func.__doc__
     dec_func.__dict__ = func.__dict__
     return dec_func
 
 class decorator(object):
     """General purpose decorator factory: takes a caller function as
 input and returns a decorator. A caller function is any function like this::
 
     def caller(func, *args, **kw):
         # do something
         return func(*args, **kw)
     
 Here is an example of usage:
 
     >> @decorator
     .. def chatty(f, *args, **kw):
     ..     print "Calling %r" % f.__name__
     ..     return f(*args, **kw)
     
     >> @chatty
     .. def f(): pass
     ..
     >> f()
     Calling 'f'
     """
     def __init__(self, caller):
         self.caller = caller
     def __call__(self, func):
         return _decorate(func, self.caller)
 
 
 #</decorators.py>

The possibilities of this pattern are endless.

A deferred decorator
-----------------------

You want to execute a procedure only after a certain time delay (for instance
for use within an asyncronous Web framework)::


 #<deferred.py>
 "Deferring the execution of a procedure (function returning None)"

 import threading
 from decorators import decorator

 def deferred(nsec):
     def call_later(func, *args, **kw):
         return threading.Timer(nsec, func, args, kw).start()
     return decorator(call_later)

 @deferred(2)
 def hello():
     print "hello"

 if __name__ == "__main__":
     hello()    
     print "Calling hello() ..."
    

 #</deferred.py>

 $ python deferred.py

Show an example of an experimental decorator based web framework
(doctester_frontend).
 
Part II: metaclasses
++++++++++++++++++++++++++++++++++++++++++++++++++

Metaclasses are there! Consider this example from a recent post on c.l.py::

 #<BaseClass.py>

 class BaseClass(object):
    "Do something"

 #</BaseClass.py>

>>> import BaseClass # instead of 'from BaseClass import BaseClass'
>>> class C(BaseClass): pass
...
Traceback (most recent call last):
  ...
TypeError: Error when calling the metaclass bases
    module.__init__() takes at most 2 arguments (3 given)

The reason for the error is that class ``C(BaseClass): pass`` is
actually calling the ``type`` metaclass with three arguments::

  C = type("C", (BaseClass,), {})

``type.__new__`` tries to use ``type(BaseClass)`` as metaclass,
but since BaseClass here is a module,  and ``ModuleType`` is not
a metaclass, it cannot work. The error message reflects a conflict with 
the signature of ModuleType which requires two parameters and not three.

So even if you don't use them, you may want to know they exist.

Rejuvenating old-style classes
--------------------------------------------

>>> class Old: pass
>>> print type(Old)
<type 'classobj'>

>>> __metaclass__ = type # to rejuvenate class
>>> class NotOld: pass
...
>>> print NotOld.__class__
<type 'type'>

A typical metaclass example: MetaTracer
----------------------------------------

::

 #<metatracer.py>

 import inspect
 from decorators import decorator

 @decorator
 def traced(meth, *args, **kw):
     cls = meth.__cls__
     modname = meth.__module__ or cls.__module__
     print "calling %s.%s.%s" % (modname, cls.__name__, meth.__name__)
     return meth(*args, **kw)

 class MetaTracer(type):            
     def __init__(cls, name, bases, dic):
         super(MetaTracer, cls).__init__(name, bases, dic)
         for k, v in dic.iteritems():
             if inspect.isfunction(v):
                 v.__cls__ = cls # so we know in which class v was defined
                 setattr(cls, k, traced(v))

 #</metatracer.py>

Usage: exploring classes in the standard library

::

 #<dictmixin.py>

 from metatracer import MetaTracer
 from UserDict import DictMixin

 class TracedDM(DictMixin, object):
     __metaclass__ = MetaTracer
     def __getitem__(self, item):
         return item
     def keys(self): 
         return [1,2,3]

 #</dictmixin.py>

>>> from dictmixin import TracedDM
>>> print TracedDM()
calling dictmixin.TracedDM.keys
calling dictmixin.TracedDM.__getitem__
calling dictmixin.TracedDM.__getitem__
calling dictmixin.TracedDM.__getitem__
{1: 1, 2: 2, 3: 3}

Real life example: check overriding
-------------------------------------
::

 #<check_overriding.py>

 class Base(object):
     a = 0

 class CheckOverriding(type):
     "Prints a message if we are overriding a name."
     def __new__(mcl, name, bases, dic):
         for name, val in dic.iteritems():
             if name.startswith("__") and name.endswith("__"): 
                 continue # ignore special names
             a_base_has_name = True in (hasattr(base, name) for base in bases)
             if a_base_has_name:
                 print "AlreadyDefinedNameWarning: " + name
         return super(CheckOverriding, mcl).__new__(mcl, name, bases, dic)

 class MyClass(Base):
     __metaclass__ = CheckOverriding
     a = 1

 class ChildClass(MyClass):
     a = 2

#</check_overriding.py>

>>> import check_overriding
AlreadyDefinedNameWarning: a
AlreadyDefinedNameWarning: a

LogFile
---------------------------------------------
::

 #<logfile.py>

 import subprocess

 def memoize(func):
     memoize_dic = {}
     def wrapped_func(*args):
         if args in memoize_dic:
             return memoize_dic[args]
         else:
             result = func(*args)
             memoize_dic[args] = result
             return result
     wrapped_func.__name__ = func.__name__
     wrapped_func.__doc__ = func.__doc__
     wrapped_func.__dict__ = func.__dict__
     return wrapped_func

 class Memoize(type): # Singleton is a special case of Memoize
     @memoize
     def __call__(cls, *args):
         return super(Memoize, cls).__call__(*args)

 class LogFile(file):
     """Open a file for append."""
     __metaclass__ = Memoize
     def __init__(self, name = "/tmp/err.log"):
         self.viewer_cmd = 'xterm -e less'.split()
         super(LogFile, self).__init__(name, "a")

     def display(self, *ls):
         "Use 'less' to display the log file in a separate xterm."
         print >> self, "\n".join(map(str, ls)); self.flush()
         subprocess.call(self.viewer_cmd + [self.name])

     def reset(self):
         "Erase the log file."
         print >> file(self.name, "w")

 if __name__ == "__main__": # test
     print >> LogFile(), "hello"
     print >> LogFile(), "world"
     LogFile().display()

 #</logfile.py>

 $ python logfile.py

Cooperative hierarchies
------------------------------

::

 #<cooperative_init.py>
 
 """Given a hierarchy, makes __init__ cooperative.
 The only change needed is to add a line
 
    __metaclass__ = CooperativeInit
 
 to the base class of your hierarchy."""
 
 from decorators import decorator 

 class CooperativeInit(type):
     def __init__(cls, name, bases, dic):

         @decorator
         def make_cooperative(__init__, self, *args, **kw):
             super(cls, self).__init__(*args, **kw)
             __init__(self, *args, **kw)

         __init__ = dic.get("__init__")
         if __init__:
             cls.__init__ = make_cooperative(__init__)

 class Base:
     __metaclass__ = CooperativeInit
     def __init__(self):
         print "B.__init__"

 class C1(Base):
     def __init__(self):
         print "C1.__init__"

 class C2(Base):
    def __init__(self):
        print "C2.__init__"

 class D(C1, C2):
     def __init__(self):
         print "D.__init__"

 #</cooperative_init.py>

>>> from cooperative_init import D
>>> d = D()
B.__init__
C2.__init__
C1.__init__
D.__init__

Metaclass-enhanced modules
----------------------------------------------------------------

::

 #<import_with_metaclass.py>
 """
 ``import_with_metaclass(metaclass, modulepath)`` generates
 a new module from and old module, by enhancing all of its classes.
 This is not perfect, but it should give you a start."""
 
 import os, sys, inspect, types

 def import_with_metaclass(metaclass, modulepath):
     modname = os.path.basename(modulepath)[:-3] # simplistic
     mod = types.ModuleType(modname)
     locs = dict(
         __module__ = modname,
         __metaclass__ = metaclass,
         object = metaclass("object", (), {}))
     execfile(modulepath, locs)
     for k, v in locs.iteritems():
         if inspect.isclass(v): # otherwise it would be "__builtin__"
             v.__module__ = "__dynamic__"
         setattr(mod, k, v)
     return mod
  
#</import_with_metaclass.py>

>>> from import_with_metaclass import import_with_metaclass
>>> from metatracer import MetaTracer
>>> traced_optparse = import_with_metaclass(MetaTracer, 
...     "/usr/lib/python2.4/optparse.py")
>>> op = traced_optparse.OptionParser()
calling __dynamic__.OptionParser.__init__
calling __dynamic__.OptionContainer.__init__
calling __dynamic__.OptionParser._create_option_list
calling __dynamic__.OptionContainer._create_option_mappings
calling __dynamic__.OptionContainer.set_conflict_handler
calling __dynamic__.OptionContainer.set_description
calling __dynamic__.OptionParser.set_usage
calling __dynamic__.IndentedHelpFormatter.__init__
calling __dynamic__.HelpFormatter.__init__
calling __dynamic__.HelpFormatter.set_parser
calling __dynamic__.OptionParser._populate_option_list
calling __dynamic__.OptionParser._add_help_option
calling __dynamic__.OptionContainer.add_option
calling __dynamic__.Option.__init__
calling __dynamic__.Option._check_opt_strings
calling __dynamic__.Option._set_opt_strings
calling __dynamic__.Option._set_attrs
calling __dynamic__.OptionContainer._check_conflict
calling __dynamic__.OptionParser._init_parsing_state

traced_optparse is a dynamically generated module not leaving in the
file system.

Magic properties
--------------------
::

 #<magicprop.py>

 class MagicProperties(type):
     def __init__(cls, name, bases, dic):
         prop_names = set(name[3:] for name in dic
                          if name.startswith("get")
                          or name.startswith("set"))
         for name in prop_names:
             getter = getattr(cls, "get" + name, None)
             setter = getattr(cls, "set" + name, None)
             setattr(cls, name, property(getter, setter))

 class Base(object):
     __metaclass__ = MagicProperties
     def getx(self):
         return self._x
     def setx(self, value):
         self._x = value

 class Child(Base):
     def getx(self):
         print "getting x"
         return super(Child, self).getx() 
     def setx(self, value):
         print "setting x"
         super(Child, self).setx(value) 

 #</magicprop.py>

>>> from magicprop import Child
>>> c = Child()
>>> c.x = 1
setting x
>>> print c.x
getting x
1

Hack: evil properties
------------------------------------

::

 #<evilprop.py>

 def convert2property(name, bases, d):
     return property(d.get('get'), d.get('set'),
                     d.get('del'),d.get('__doc__'))

 class C(object):
     class x:
         """An evil test property"""
         __metaclass__ = convert2property
         def get(self):
             print 'Getting %s' % self._x
             return self._x
         def set(self, value):
             self._x = value
             print 'Setting to', value

 #</evilprop.py>

>>> from evilprop import C
>>> c = C()
>>> c.x = 5
Setting to 5
>>> c.x
Getting 5
5
>>> print C.x.__doc__
An evil test property

Why I suggest *not* to use metaclasses in production code
---------------------------------------------------------

 + there are very few good use case for metaclasses in production code
   (i.e. 99% of time you don't need them)

 + they put a cognitive burden on the developer;

 + a design without metaclasses is less magic and likely more robust;

 + a design with metaclasses makes it difficult to use other metaclasses 
   for debugging.

As far as I know, string.Template is the only metaclass-enhanced class
in the standard library; the metaclass is used to give the possibility to
change the defaults::

    delimiter = '$'
    idpattern = r'[_a-z][_a-z0-9]*'

in subclasses of Template.

>>> from string import Template
>>> from metatracer import MetaTracer
>>> class TracedTemplate(Template):
...     __metaclass__ = MetaTracer
...
Traceback (most recent call last):
  ...
TypeError: Error when calling the metaclass bases
    metaclass conflict: the metaclass of a derived class must be a (non-strict) subclass of the metaclasses of all its bases

Solution: use a consistent metaclass

>>> class GoodMeta(MetaTracer, type(Template)): pass
...
>>> class TracedTemplate(Template):
...     __metaclass__ = GoodMeta


Is there an automatic way of solving the conflict?
---------------------------------------------------------------------

Yes, but you really need to be a metaclass wizard.

http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/204197

>>> from noconflict import classmaker
>>> class TracedTemplate(Template):
...     __metaclass__ = classmaker((MetaTracer,))
>>> print type(TracedTemplate)
<class 'noconflict._MetaTracer_TemplateMetaclass'>

::

 #<noconflict.py>

 import inspect, types, __builtin__
 from skip_redundant import skip_redundant

 memoized_metaclasses_map = {}

 # utility function
 def remove_redundant(metaclasses):
    skipset = set([types.ClassType])
    for meta in metaclasses: # determines the metaclasses to be skipped
        skipset.update(inspect.getmro(meta)[1:])
    return tuple(skip_redundant(metaclasses, skipset))

 ##################################################################
 ## now the core of the module: two mutually recursive functions ##
 ##################################################################

 def get_noconflict_metaclass(bases, left_metas, right_metas):
     """Not intended to be used outside of this module, unless you know
     what you are doing."""
     # make tuple of needed metaclasses in specified priority order
     metas = left_metas + tuple(map(type, bases)) + right_metas
     needed_metas = remove_redundant(metas)

     # return existing confict-solving meta, if any
     if needed_metas in memoized_metaclasses_map:
       return memoized_metaclasses_map[needed_metas]
     # nope: compute, memoize and return needed conflict-solving meta
     elif not needed_metas:         # wee, a trivial case, happy us
         meta = type
     elif len(needed_metas) == 1: # another trivial case
        meta = needed_metas[0]
     # check for recursion, can happen i.e. for Zope ExtensionClasses
     elif needed_metas == bases: 
         raise TypeError("Incompatible root metatypes", needed_metas)
     else: # gotta work ...
         metaname = '_' + ''.join([m.__name__ for m in needed_metas])
         meta = classmaker()(metaname, needed_metas, {})
     memoized_metaclasses_map[needed_metas] = meta
     return meta

 def classmaker(left_metas=(), right_metas=()):
    def make_class(name, bases, adict):
        metaclass = get_noconflict_metaclass(bases, left_metas, right_metas)
        return metaclass(name, bases, adict)
    return make_class

 #################################################################
 ## and now a conflict-safe replacement for 'type'              ## 
 #################################################################
 
 __type__=__builtin__.type # the aboriginal 'type'
 # left available in case you decide to rebind __builtin__.type

 class safetype(__type__):
     # this is REALLY DEEP MAGIC
     """Overrides the ``__new__`` method of the ``type`` metaclass, making the
     generation of classes conflict-proof."""
     def __new__(mcl, *args):
         nargs = len(args)
         if nargs == 1: # works as __builtin__.type
             return __type__(args[0]) 
         elif nargs == 3: # creates the class using the appropriate metaclass
             n, b, d = args # name, bases and dictionary
             meta = get_noconflict_metaclass(b, (mcl,), ()) 
             if meta is mcl: # meta is trivial, dispatch to the default __new__
                 return super(safetype, mcl).__new__(mcl, n, b, d)
             else: # non-trivial metaclass, dispatch to the right __new__
                 # (it will take a second round) # print mcl, meta
                 return super(mcl, meta).__new__(meta, n, b, d)
         else:
             raise TypeError('%s() takes 1 or 3 arguments' % mcl.__name__)

 #</noconflict.py>
