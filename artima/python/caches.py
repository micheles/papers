"""
I work as an enterprise programmer and every day I have to face 
problems typical of the enterprise context. Here is an example
from a ticket I got assigned yesterday. As everybody does, we employ
caches in various places in our code base for performance reasons. As
everybody does, we have problems with them. Let me explain the specific
issue in the ticket. We have literally a dozen or more of different kind
of caches: in somes place we use simple dictionaries, in other places
we use slightly smarter data structures and in some places we use a wrapper
over memcached. The caches have been implemented by different
programmers during a period of years; there are caches at the Python
level and caches at the C++ level. The caches are cleared in a few
spots, and in particular there is code like this::

  if number_objects_allocated > MAX_OBJECTS:
       cpp_cache.reset()
       python_cache.clear()
       the_other_python_cache.reset()
       the_other_other_python_cache.flush()
       ... etc etc

Actually it is more complicated than that.  Notice that the caches do
not share a common interface: sometimes ``.clear`` is called
``.reset``, sometimes ``.flush``, sometimes ``.flush_all``, etc. The
usual things that happens when you have different programmers working
in different groups in different times, deadlines and lack of code
review.  The problem is that nobody remembers what are all the caches
involved exactly. It has already happened twice that somebody forgot
to clear a cache that should have been cleared, resulting in errors in
far way portions of the code.

I got assigned the task of finding which were the relevant caches (and
this is the difficult part, since it involves spelunking through
hundreds of thousands of lines of code and asking people who do not
remember who wrote what) and to unify the caching mechanism, making it
more robusts. This is the easier part, and I will talk about this part
here. My ticket suggested to solve the problem with a metaclass
trick, to make it sure that all cache instances were registered as
soon as created, and to make it possible to clear all caches in a
single step.
The requirements made me remember an old recipe of mine, published on
the `Python Cookbook`_ a couple of years ago, the AutoClose_ recipe.
It was rather easy to convert the recipe to provide the needed
functionality. The code is given below, here I will discuss how it
works.

The idea is to track down in our codebase the places where our caches
are *defined*, which is much easier than finding the places were the
cache are *used*. Having found a pre-existing cache class, I can add
the tracking capability to it simply by adding a line ``__metaclass__
= AutoClearMeta`` to it, which is very little invasive. In some places
I also need to add an alias method: for instance if the class has a
``reset`` method instead of a ``clear`` method I can just add the
alias ``clear = reset``.  In many places the caches are simple Python
dictionaries: in such situations I just need to replace statements
like ``_cache = {}`` with ``_cache = DictCache()`` where ``DictCache``
is just an AutoClearMeta-augmented dictionary, defined in the obvious
way:

$$DictCache

The advantage of doing so is that now I can reduce the clearing code to
the following three lines::

  if number_objects_allocated > MAX_OBJECTS:
       cpp_cache.clear() # taking care of the C++ caches is not my job
       python_cache_class.clear_all()

When in the future people will add more caches, this code will stay
unchanged, since the metaclass takes care of registering all the
caches for us. Some care is required of course, since the programmers
are expected to use a cache class augmented by ``AutoClearMeta``.
However, even if they forget to do so and they use their own class, 
the fix is just one line of code.

The design here is quite elegant. If you have a set of caches that should be
cleared at the same time, the solution is to make them all instances of
the same subclass ``S``: calling ``S.clear_all()`` will clear all of them
in a row. Morever, if ``S1`` is a subclass of ``S``, ``S.clear_all()``
will clear all instances of ``S1`` too, whereas  ``S1.clear_all()``
will clear only the instances of ``S1`` and its subclasses (if any).
On the other hand, if you have classes which should *not* be cleared
at the same time, just make them instances of a different cache class,
since caches in different hierarchies are cleared out independently.

I thought I would share this recipe, since it may be of use to somebody, and
also because real life usages of metaclasses are so rare that
they are worth mention. Here is the code for the AutoClearMeta metaclass:

$$AutoClearMeta

.. _Python Cookbook: http://code.activestate.com/recipes/langs/python/
.. _AutoClose: http://code.activestate.com/recipes/523007
"""

class AutoClearMeta(type):
   """
   Metaclass that tracks the instances of its instances and clears them 
   in reverse instantiation order. Requires classes with a .clear method.
   It is possible to clear absolutely everything with

   for cls in AutoClearMeta.autoclearclasses:
       cls.clear_all()
   """
   autoclearclasses = []

   def __new__(mcl, name, bases, dic):
       cls = super(AutoClearMeta, mcl).__new__(mcl, name, bases, dic)
       cls.clear # assert the method clear exists
       cls._instances = []
       mcl.autoclearclasses.append(cls)
       return cls

   def __call__(cls, *args, **kw):
       # tracks the instances of the instances
       self = super(AutoClearMeta, cls).__call__(*args, **kw)
       cls._instances.append(self)
       return self

   def clear_all(cls):
       "Recursively clear all instances of cls and its subclasses"
       for subc in cls.__subclasses__(): # direct proper subclasses
           subc.clear_all()
       for obj in reversed(cls._instances):
           obj.clear()
 
class DictCache(dict):
    "An AutoClearMeta-augmented dictionary class"
    __metaclass__ = AutoClearMeta
    
if __name__ == '__main__': # test
    import logging

    class S(object):
        __metaclass__ = AutoClearMeta
        def clear(self):
            logging.warn('clearing cache %s' % self)

    class S1(S):
       pass
    class S2(S1):
       pass

    c1 = S()
    c2 = S()
    c3 = S1()
    c4 = S2()
    S.clear_all()
