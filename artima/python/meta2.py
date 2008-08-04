# -*- encoding: utf-8 -*-
r"""
A zoology of records
-----------------------------------------------------------

Classes are used to model set of objects; in the same sense, metaclasses are
used to model set of classes. For exemplification purposes, suppose we want
to define a set of record classes, all subclasses of a common mother class
``Record`` and all instances of the same metaclass ``MetaRecord``, with
a sum operator. In mathematical term our set of classes will be a monoid, i.e.
a set with an associative composition law ``+`` and an neuter, the
base class ``Record``. Python tuples are already a monoid, since the
sum of two tuples is a tuple and the empty tuple is the the neuter element,
therefore it makes sense to define our set of classes as a set of tuples.
The different between ordinary tuples and records is that records have
the concept of type: every field has a given type. Since Python is dynamically
types, it makes sense to specify the field in terms of casting functions,
taking in input one or more types (for instance integers and/or strings)
and returning a fixed type in output, or a TypError if the if the input
type is not acceptable. We will consider the following casting functions
for records of kind ``Book``:

.. code-block:: python

 def varchar(n):
     def check(x):
         s = str(x)
         if len(s) > n:
               raise TypeError('Entered a string longer than %d chars' % n)
         return s
     check.__name__ = 'varchar(%d)' % n
     return check


.. code-block:: python

 def date(x):
       if isinstance(x, datetime.date):
             x = x.isoformat()[:10]
       return datetime.date(*map(int, x.split('-')))


.. code-block:: python

 def score(x):
       if set(x) != {'*'} or len(x) > 5:
             raise TypeError('%r is not a valid score!' % x)
       return len(x)


*varchar(N)* makes sure that the string input is shorter than ``N`` characters
and for instance

>>> varchar(128)('a'*129)
Traceback (most recent call last):
  ...
TypeError: Entered a string longer than 128 chars

*date* make sure that the string in input is a data in ISO format; 
*score* will convert a string with or or more stars into an integer
number: the idea is that a book has a score in stars, for one to five.

>>> score('***')
3

>>> score('')
Traceback (most recent call last):
   ...
TypeError: '' is not a valid score!

We can now define records like the following:

.. code-block:: python

 class Book(Record):
       title_type = varchar(128)
       author_type = varchar(64)


.. code-block:: python

 class PubDate(Record):
       date_type = date


.. code-block:: python

 class Score(Record):
       score_type = score

On these records it is possible to define a sum operator taking two or more classes
and returning a new class:

>>> Book + PubDate + Score
<class Book+PubDate+Score title:varchar(128), author:varchar(64), date:date, score:score>

It will be possible to verify the associativity:

>>> (Book + PubDate) + Score == Book + (PubDate + Score)
True

and the existence of the neuter:

>>> Book + Record == Book
True

>>> Record + Book == Book
True

These properties at the class level correspond to analogous properties at the instance
level. Consider for instance the null record

>>> null = Record() 
>>> null
<Record >

a un record di tipo *Book*
   
>>> b = Book('Putting Metaclasses to Work', 'Ira Forman')
>>> b
<Book title=Putting Metaclasses to Work, author=Ira Forman>

and a record of kind *PubDate*:

>>> d = PubDate('1998-10-01')

You can see that *null* is an identity:

>>> b + null == null + b == b
True

Here is an example of sum of nontrivial records:

>>> s = b + d
>>> s
<Book+PubDate title=Putting Metaclasses to Work, author=Ira Forman, date=1998-10-01>

You can access the fields by name: 

>>> s.title, s.author, s.date
('Putting Metaclasses to Work', 'Ira Forman', datetime.date(1998, 10, 1))

How does it work?

The method ``__add__`` of the base class ``Record`` determines the classes
of the records in input, builds the class of the record in output by
using ``MetaRecord``, and instantiate it with the right values.
In particolar, if the classes in input take ``N`` and ``M`` parameters
respectively (in our case ``N=2`` and ``M=1``) the class in out will take ``N+M``
parameters (in our case title, author and publication date).
Here is the code for the base class ``Record``:

$$Record

Since the metaclass is smart enough to keep a register of its instances,
if  class compatible with the class of the record in output alread
exists, it is reused: in other words, we don't need to create new
classes at each sum. ``MetaRecord`` defines an equivalence relation
between its instances by defining the ``__eq__`` method:
two record classes are considered equivalente (compatible)
if they have the same fields in the same order. Another metaclass
magic is the validation of the parameters before instantiation
of a concrete record, implemented by overriding by metaclass ``__call__``
method. Here we check that we are passing the right number of parameters
and we convert them into the right types or we raise an exception.
Therefore the ``Record`` subclass can assume to get the right
parameters and its ``__init__`` method is simple. The record
fields are accessible by name since the metaclass defines suitable
properties automatically. Finally, the metaclass redefine the
``+`` operator by implementing the ``__add__`` method in terms
of subclassing, so that

>>> BookWithScore = Book + Score

is the same than

>>> class BookWithScore2(Book, Score):
...      pass

>>> BookWithScore2 == BookWithScore
True

There is a difference, though: the sum reuses pre-existing classes if
possible, whereas inheritance always creates new classes, as customary
in Python. Here is the code for ``MetaRecord``:

$$MetaRecord

``check_disjoint`` and ``getslots`` are utility functions:

$$check_disjoint
$$getslots

Warning
-----------------------------------------------

The code I presented here is intended for pedagogical purposes. Generally speaking
I advice not to use metaclasses in production, since more often than not there
are simpler and less magical alternatives available. In particular the ability
to use class decorators in Python 2.6/3.0 has substantially reduced the
number of good use cases for metaclasses. You should never use a metaclass
to introduce a feature that should not be inherited.
If you want to redefine
binary operators like ``+``, ``-``, ``*``, ``/``, ``==`` or ``!=`` etc
at the class level there is other way than via a custom metaclass, however
you could also consider the idea of defining suitable top-level functions
(plus, minus, mul, div, eq, neq) operating on classes following some
specific interface. 
"""

from meta1 import odict

def check_disjoint(names):
      storage = set()
      for name in names:
            if name in storage:
                  raise NameError('Field %r occurs twice!' % name)
            else:
                  storage.add(name)

def getslots(bases):
      return sum((getattr(b, 'all_slots', ()) for b in bases), ())

class MetaRecord(type):
      _repository = {} # repository of classes already instantiated

      @classmethod
      def __prepare__(mcl, name, bases):
            return odict()
 
      def __new__(mcl, name, bases, odic):
            entered_slots = tuple((n,v) for n, v in odic.items() if n.endswith('_type'))
            all_slots = getslots(bases) + entered_slots
            check_disjoint(n for (n, f) in all_slots) # check the field names are disjoint
            odic['all_slots'] = all_slots
            odic['all_names'] = tuple(n[:-5] for n, f in all_slots)
            odic['all_fields'] = fields = tuple(f for n, f in all_slots)
            cls = super().__new__(mcl, name, bases, odic)
            mcl._repository[fields] = cls
            for i, (n, v) in enumerate(all_slots):
                  setattr(cls, n[:-5], property(lambda self, i=i: self[i]))
            return cls
 
      def __eq__(cls, other):
            return cls.all_fields == other.all_fields
  
      def __neq__(cls, other):
            return cls.all_fields != other.all_fields
 
      def __call__(cls, *values):
            expected = len(cls.all_slots)
            passed = len(values)
            if passed != expected:
                  raise TypeError('You passed %d parameters, expected %d' %
                                  (passed, expected))
            vals = [f(v) for (n, f), v in zip(cls.all_slots, values)]
            return super().__call__(vals)

      def __add__(cls, other):
            name = '%s+%s' % (cls.__name__, other.__name__)
            fields = tuple(f for n, f in getslots((cls, other)))
            try: # retrieve from the repository an already generated class
                  return cls._repository[fields]
            except KeyError:
                  return type(cls)(name, (cls, other), {})
 
      def __repr__(cls):
            slots = ['%s:%s' % (n[:-5], f.__name__) for (n, f) in cls.all_slots]
            return '<%s %s %s>' % ('class', cls.__name__, ', '.join(slots))

class Record(tuple, metaclass=MetaRecord):
    "Base record class, also working as identity element"

    def __add__(self, other):
           cls = type(self) + type(other)
           return cls (*super().__add__(other))

    def __repr__(self):
          slots = ['%s=%s' % (n, v) for n, v in zip(self.all_names, self)]
          return '<%s %s>' % (self.__class__.__name__, ', '.join(slots))

