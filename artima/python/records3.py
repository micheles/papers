r"""\
In the first two installaments of this series
I discussed how to read and process homogeneous records.
In this final installment I will discuss non-homogeneous records and
we will devise a small framework to convert text records into CSV, HTML,
XML or other formats. *En passant*, I will discuss various object oriented 
techniques and patterns.

.. figure:: http://www.phyast.pitt.edu/~micheles/python/patchwork1.jpg
 :width: 300

 Fig 1: object-oriented design

A micro-framework to convert records into text
-------------------------------------------------------------------

It is well know that I am not a framework lower and there are certainly
many Python programmers sharing this attitude, starting from Guido.
Actually, my dislike of frameworks is inversely proportional to their
size: I hate the mega-frameworks, I tolerate the medium-sized framework
and I like enough the micro-frameworks. In this installment I will define
a micro-framework to render non-homogeneous records into text. The
framework is based on the `template pattern`_: in order to define a
renderer class, the programmer inherits from a mother class ``RecordRenderer``
and fills in the rendering methods: then the framework with automatically
call them but without too much magic.

This approach is acceptable only when the base class is simple: it is much
less acceptable when you start already from a deep hierarchy. For me a
hierarchy is deep if there are more than two levels: if looking at
mother and children is not enough, and I am forced to look even and
the grand-parent classes, the framework is already too complex.

Inheritance-based frameworks have the tendency to go out of control,
because it become natural to extend the hierarchy too much. In 
traditional object-oriented languages it is quite natural to use
inheritance, but as I said elsewhere one should always keep in mind
that alternative are always possible (a notable new language *without*
inheritance is Go).

Anyway, one should not fight the language she is using: in Python the
`template pattern`_ is a perfectly reasonable approach.

.. figure:: http://www.phyast.pitt.edu/~micheles/python/patchwork2.jpg

 Fig 2: the *template pattern*

To convert into text a non-homogenous 
record with *N* fields requires in general *N+1*
functions: *N* functions to convert the fields and a function to convert
the full record. It is natural to group the needed functions as method
of a renderer class: the *N* field-associated rendering functions will
be methods converting values into strings, whereas the *N+1* function
will be a ``.render`` method converting the record of strings so obtained
into a single string. We will use a base class called ``RecordRendererABC``,
where the ABC suffix means *Abstract Base Class*. 

I should point out that an Abstract Base Class in Python can provide
concrete methods to its subclasses and therefore the meaning of ABC
in Python is different than in C++/Java: a Python ABC is a mixin class,
which can provide implementation; it is not necessarily pure interface.

For instance, suppose we want to convert an Article record 

 ``Article = namedtuple("Article", "title author pubdate")``

into CSV format.

How do we proceed? First of all we define a suitable subclass of
``RecordRendererABC``:

$$CSVArticleRenderer

Notice that ``CSVArticleRenderer`` defines a ``.schema`` class
attribute, a namedtuple containing the names of the rendering methods.

In this example both title and author are converted by using the ``.str``
method, inherited from the base class, whereas the publication date
is converted by using the ``.isodate`` method, which is defined
directly in the ``CSVArticleRenderer`` class.
The ``.render`` method is inherited and converts the input namedtuple
into a string by converting into strings the fields with the corresponding
methods and by joining the results, using a comma as separator.
Here is an example:

  >>> a = Article("test title", "test author", datetime(2008, 05, 15))
  >>> r = CSVArticleRenderer(a)

The ``.render`` method works as expected:

  >>> print r.render()
  test title,test author,2008-05-15

By default the separator (``delimiter``) is set to the empty string ''.
This is useful for implementing different renderers. For instance,
suppose we want to define a renderer converting the articles into HTML
format. Suppose we defined three CSS classes ``title``, ``author`` and
``pubdate`` to visualize the different fields in different ways, for
instance with different colors. We could define a renderer using the
CSS classes as follows:

$$HTMLArticleRenderer

Here is how the renderer works:

  >>> r = HTMLArticleRenderer(a)
  >>> print r.render()
  <span class="title">test title</span>
  <span class="author">test author</span>
  <span class="pubdate">2008-05-15</span>

Design notes
--------------------------------------------------------------------

Having discussed the usage of the framework, it is now the time to
discuss the implementation of the base class and the reasons for the
design choices I made.
Here is the source code for ``RecordRendererABC``:

$$RecordRendererABC

Let me start from the constructor. The ``__init__`` methods accepts in input
a single argument, a sequence with length equal to the length of the schema.
The input sequence *is not required to be a namedtuple*: there is no type check
such as ``isinstance(input, self.schema.__class__)``.
A type check here would be a design mistake, since it would restrict without
reason the field of applicability of the renderer and it would force
the users to use type converted without need. The only requirement for
the ``input`` object is that ``zip(self.schema, input)`` must not raise
an exception: in other words, it is enough that ``input`` had the right
length.

Actually ``zip(self.schema, input)`` would not raise an error even if
``input`` had a different length. This is potentially dangerous.
For instance, imagine that for some reason (say a programmer error)
we passed a sequence of length zero: then ``zip(self.schema, input)`` 
would silently return an empty list. Since *errors should never pass silently*,
I decided to add a check on the length: in this way if there is an error
we see it immediately, at instantiation time, and not too late,
when we start iterating on the renderer. It is always better to discover
errors early.

On the other hand, it is best to not exaggerate with the checks. For instance,
if ``.input`` is a list, it is theoretically possible for an evil programmer
to modify the list *after* instantiation, by adding or removing elements.
Then ``zip(self.schema, input)`` could behave in an unexpected way.
However, there is no way to protect against evil (or just bad) programmers.
Even if we replaced ``.input`` with a tuple, which is immutable, its
size could always be changes, simply by overriding the ``.input``
attribute after instantiation.

The Python way is to limit the checks to the one dictated from common
sense, intended to limit accidental errors which are likely to happen:
for the rest, the attitude is to trust the user. Checks motivated by
paranoia and lack of trust in the user are not to be introduced, since
in a dynamic language the user can do whenever she wants anyway.
The attitude is mutuated from the `spirit of C`_ (*trust the programmer*).
According to this maxim I decided not to add additional checks.

In special cases (for instance if you are implementing a subclass of
``RecordRendererABC`` which requires for ``.input`` to be a record)
it may be sensible to introduce some additional check. For instance
you may want to ensure that ``.input`` be a record with the right
fields. However, even in this case it is best not to introduce a
type check like
``isinstance(input, self.schema.__class__)``; you can instead
use a lighter check like ``input._fields == self.schema.fields``: 
in this way any object with the right fields would be accepted,
not use a namedtuple. The basic idea is to follow the 
`duck typing`_ principle: don't be too picky and
accept as good anything with the needed
attributes.

In this logic you may want to enlarge even more the field of
acceptable objects: for instance a dictionary-like object
with the right keys could act as a substitute for a record.
We could implement such feature by adding an ``if`` in the
``__init__`` method, by introducing a special case when the input object
is a dictionary. But that would be bad programming: the point of object
oriented programming is to avoid ``ifs`` and to replace them with methods.
In our example, we should remember that Python provides a *classmethod*
construct, which *raison d'etre* is exactly to manage this use case:
it allows the programmer to define alternate constructors, without
the need for complicating the ``__init__`` method. Using alternate
constructors is called `factory method pattern`_ and it is one of
the basic techniques of OOP. The advantages are clear, expecially
in terms of simplicity and easy of maintenance, but also from the
point of view of code reuse and extensibility.

.. figure:: http://www.phyast.pitt.edu/~micheles/python/patchwork3.jpg
 
 Fig 3: the *factory method pattern*

In our example dictionaries are rendered through the ``.frommap``
classmethod:

    >>> r = CSVArticleRenderer.frommap(dict(
    ...     title="test title", author="test author", 
    ...     pubdate=datetime(2008, 05, 15)))

There is also a ``.fromobj`` classmethod accepting in input any
object with a set of attributes which is a superset (proper or
improper) of the schema's attributes. This is pure *duck typing*.
If the object lacks an attribute, we will get an ``AttributeError``
at instantiation time, an absolutely clear and telling error message;
on the other hand, if the object has enough attributes, it will be
automatically converted into a namedtuple.

The base class also defines the special methods ``__iter__`` and ``__len__``:
therefore each rendered instance is a sequence of fixed length and can be
passed in input to another renderer. In other words, renderers are
composable in the functional sense.

Renderers are actually homogeneous records with fields which are strings
and can be passed to the ``HtmlTable`` object defined in the previous
installment. It is trivial to convert a rendered into a list of strings:
thanks to the ``__iter__`` method, ``list(renderer)`` works as expected
(idem for ``tuple(renderer)`` and ``len(renderer)``). `list``, ``tuple`` and
``len`` are actually builtin generic functions which play well with
*duck typing* and are definible for any custom object.

It was good to discuss what was implemented into ``RecordRendererABC``;
it is also interesting to discuss what was *not* implemented.
In particular, I did not implement the renderers are namedtuples.
I wanted to avoid the *blob* antipattern_, when you have a class which
is everything to everybody. I wanted to keep namedtuples simple, without
adding any methods to them: renderers are logically an independent concept,
even if they can be converted into namedtuples, being iterable.

.. figure:: http://www.phyast.pitt.edu/~micheles/python/blob.jpg
 :width: 350

 Fig 4: the *blob antipattern*

I did define ``CSVArticleRenderer`` and ``HTMLArticleRenderer`` as
subclasses of ``RecordRendererABC``. An alternate design could have
introduced different abstract intermediate subclasses, depending on the output
format: for instance ``CSVRecordRenderer``, ``HTMLRecordRenderer``,
``XMLRecordRenderer``, etc. However I have decided of following strictly
the rule that *flat is better than nested*, and to keep the hierachies
as short as possible.
Actually in Python 2.6+ one could define three abstract interfaces
``CSVRecordRenderer``, ``HTMLRecordRenderer`` and ``XMLRecordRenderer``
and one could register her concrete classes with such interfaces: this
can be done without using inheritance and by keeping the hierarchy flat.

.. _spirit of C: http://www.artima.com/cppsource/spiritofc.html
.. _duck typing: http://en.wikipedia.org/wiki/Duck_typing
.. _factory method pattern: http://en.wikipedia.org/wiki/Factory_method_pattern
.. _antipattern: http://en.wikipedia.org/wiki/Antipattern
.. _template pattern: http://en.wikipedia.org/wiki/Template_pattern
"""
import os, cgi
from datetime import datetime
from tabular_data import headtail
from collections import namedtuple

# in Python 2.6 use abstractmethod, abstractproperty instead
class notimplemented(object):
    "Descriptor raising a meaningful error message for nonoverridden attributes"
    def __init__(self, message):
        self.message = message
    def __get__(self, obj, objcls=None):
        raise NotImplementedError(self.message)

class RecordRendererABC(object):
    schema = () # a namedtuple specifying the names of the converters
    delimiter = ''
    
    @classmethod
    def frommap(cls, kw):
        return cls(cls.schema.__class__(**kw))

    @classmethod
    def fromobj(cls, obj):
        Schema = cls.schema.__class__
        nt = Schema._make(getattr(obj, field) for field in Schema._fields)
        return cls(nt)

    def __init__(self, input):
        li, ls = len(input), len(self)
        if li != ls:
            raise TypeError('%s has %d fields, expected %d' % (input, li, ls))
        self.input = input
        
    def __iter__(self):
        for convertername, value in zip(self.schema, self.input):
            yield getattr(self, convertername)(value)
    
    def __len__(self):
        return len(self.schema)

    def str(self, value):
        return str(value)

    def render(self):
        return self.delimiter.join(self)
    
Article = namedtuple("Article", "title author pubdate")

class CSVArticleRenderer(RecordRendererABC):
    schema = Article("str", "str", "isodate")
    delimiter = ','
    def isodate(self, date):
        return date.isoformat()[:10]

class HTMLArticleRenderer(RecordRendererABC):
    schema = Article(title='title', author='author', pubdate="pubdate")
    delimiter = '\n'
    def title(self, title):
        return '<span class="title">%s</span>' % cgi.escape(title)
    def author(self, author):
        return '<span class="author">%s</span>' % cgi.escape(author)
    def pubdate(self, date):
        return '<span class="pubdate">%s</span>' % date.isoformat()[:10]
    
# todo: xml.escape    
def to_xml(ntuple):
    name = ntuple.__class__.__name__
    xml = ['<%s>' % name]
    for i, field in enumerate(ntuple._fields):
        xml.append("<%s>%s</%s>" % (field, ntuple[i], field))
    xml.append('</%s>' % name)
    return os.linesep.join(xml)

if __name__ == '__main__':
    import doctest; doctest.testmod()
    HTMLArticleRenderer.fromobj(Article("a",'b', datetime.today()))
