# -*- encoding: utf-8 -*-
r"""
I head for the first time the word "metaclass" in 2002, when I started studying
Python. I was very active in the Python newsgroup then, and I received from
David Mertz an invitation to write a paper about metaclasses together.
That was my very first paper outside Physics and since then I have a
particular affection for the subject. At the time subject was hot
since the metaclass mechanism had been just reformed an simplified
(that was around the time Python 2.2 came along). Nowadays the subject
is still hot since with Python 3.0 coming in October we will have
another reform of the metaclass mechanism. Therefore, I could not
miss the change to write something about the changes.

Introduction
-------------------------------------

I assume here my readers to be familiar with metaclass programming
in Python, at least as it is intended in recent versions of Python
(from Python 2.2 to Python 2.5). If you need to refresh your
memory, my advice is to give a look to the metaclass trilogy
David and I wrote for IBM DeveloperWorks,
*Metaclass programming in Python*, parts 1_, 2_ e 3_ .

A metaclass is nothing else than a subclass of the builtin metaclass ``type``:

>>> class DoNothingMeta(type): # a silly metaclass
...       pass

A metaclass can be instantiated by specifyinga string *name*,
a tuple of classes *bases* and a dictionary *dic*:

>>> C = DoNothingMeta('C', (object,), {})

The metaclass instance is a class named *C*, with parent *object* and empty
dictionary. In Python 3.0 everything remain the same: the only change
is in the *syntactic sugar*. In Python 2.2+ you have at your disposition
the so-called ``__metaclass__`` hook, so that the previous line can
be written also

>>> class C(object): __metaclass__ = DoNothingMeta

In Python 3.0+, instead, you can use a somewhat nicer syntax:

>>> class C(object, metaclass=DoNothingMeta): pass

The important thing to notice is that in Python 3.0 you can write

>>> class C(object): __metaclass__ = DoNothingMeta

*but the hook is not recognized*: Python 3.0 will just add a ``__metaclass__``
attribute to your class, but the attribute will not treated in any special
way and in particular your class will not be magically converted in an instance
of the metaclass: the class of ``C`` will stay ``type`` and not ``DoNothingMeta``.

>>> type(C) is DoNothingMeta
False

The fundamental change
---------------------------------------------------------------

The syntactic changes, however useful, are still cosmetics. The substantial
change between Python 2.2 and Python 3.0 is *semantic*: in Python 3.0
the dictionary passed to the metaclass can be replaced with a generic
object with a ``__setitem`` method. In particular, that means that you
can pass to a metaclass an ordered dictionary instead of a ordinary
dictionary and therefore it is possible to jeep the ordering of the
declarations in the class body, a much desired feature for metaclass
practictioners.
For instance, suppose we want to define a ``Book`` class with fields
``title`` and ``author`` (in that order).
In Python 2.2 we would
be forced to repeat the names of the fields:

.. code-block:: python

 class BookPython22(Record):
 
     title = 'varchar(128)'
     author = 'varchar(64)'
 
     order = ['title', 'author']

The ``order`` list is redundant and annoying, but you cannot get rid of
it in Python 2.2, otherwise you would miss the relative ordering of ``title``
and ``author``. In Python 3.0 instead, the ``order`` list can be avoided,
provided we use an ordered dict.
At the moment I am writing it is not clear if an ordered dict class will enter
in the standard library of Python 3.0, therefore I will give a simple
implementation here:

$$odict

Notice that I did not define the methods ``.iterkeys()``,
``.itervalues()`` and ``.iteritems()`` since they were removed in Python 3.0.
Notice also that I wrote ``super().__setitem__(name, value)`` and not
``super(odict, self).__setitem__(name, value)`` as in Python 2.2 since
in Python 3.0 is smart enough to figure out the obvious arguments
(you can still write them explicitly if you really wish).

We still need to tell the metaclass to use an ``odict`` instead of ``dict``.
To this aim, Python 3.0 recognizes a special (class)method
``__prepare__(mcl, name, bases)`` returning the would be class dictionary.
Therefore it is enough to add a suitable ``__prepare__`` to the metaclass:

>>> class MetaBookExample(type): 
...     @classmethod
...     def __prepare__(mcl, name, bases):
...         return odict()

>>> class BookExample(metaclass=MetaBookExample):
...       title = 'Varchar(128)'
...       author = 'Varchar(64)' 

>>> vars(BookExample) # this is an odict instance
{[('__module__', '__main__'), ('__doc__', 'Un esempio'), ('title', 'Varchar(128)'), ('author', 'Varchar(64)')]}

We see here that the metaclass is also passing a couple of implicit magic attributes
``__module__`` and ``__doc__`` to the class. This is the usual  behavior even
in Python 2.2+ so it should not come as a surprise to you.

.. _1: http://www.ibm.com/developerworks/linux/library/l-pymeta.html
.. _2: http://www-128.ibm.com/developerworks/linux/library/l-pymeta2/
.. _3: http://www.ibm.com/developerworks/library/l-pymeta3.html
.. _Metaprogramming without Metaclasses: http://www.ibm.com/developerworks/library/l-pymeta3.html
"""

class odict(dict):
    "A simple implementation of ordered dicts without __delitem__ functionality"

    def __init__(self, alist=None):
        if alist is None:
            self._alist = []
        else:
            self._alist = alist
        for k, v in self._alist:
            self[k] = v

    def __setitem__(self, name, value):
        super().__setitem__(name, value)
        self._alist.append((name, value))

    def keys(self):
        return [k for (k, v) in self._alist]

    __iter__ = keys

    def values(self):
        return [v for (k, v) in self._alist]

    def items(self):
        return self._alist

    def __repr__(self):
        return '{%s}' % self._alist

if __name__ == '__main__':
    import doctest; doctest.testmod()
