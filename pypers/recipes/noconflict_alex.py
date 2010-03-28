# noconflict_alex.py

import inspect, types
try: set
except NameError:
    from sets import Set as set

memoized_metaclasses_map = {}

def uniques(sequence, skipset):
    for item in sequence:
        if item not in skipset:
            skipset.add(item)
            yield item# noconflict.py

def remove_redundant(classes):
    redundant = set([types.ClassType])
    for c in classes:
        redundant.update(inspect.getmro(c)[1:])
    return tuple(uniques(classes, redundant))

def offending_metaclass_in(metas):
    for m in metas:
        if not issubclass(m, type): return True
    return False

def _get_noconflict_metaclass(bases, left_metas, right_metas):
     # make tuple of needed metaclasses in specified priority order
     metas = left_metas + tuple(map(type, bases)) + right_metas
     needed_metas = remove_redundant(metas)

     # return existing confict-solving meta, if any
     if needed_metas in memoized_metaclasses_map:
         return memoized_metaclasses_map[needed_metas]

     # nope: compute, memoize and return needed conflict-solving meta
     if not needed_metas:         # wee, a trivial case, happy us
         meta = type
     elif len(needed_metas) == 1: # another trivial case
         meta = needed_metas[0]
     elif offending_metaclass_in(needed_metas): # es. Zope Extension Classes
         raise TypeError("Incompatible root metatypes", needed_metas)
     else: # gotta work ...          
         metaname = '_' + ''.join([m.__name__ for m in needed_metas])
         meta = classmaker()(metaname, needed_metas, {})
     memoized_metaclasses_map[needed_metas] = meta
     return meta

def classmaker(left_metas=(), right_metas=()):
     def make_class(name, bases, adict):
         metaclass = _get_noconflict_metaclass(bases, left_metas, right_metas)
         return metaclass(name, bases, adict)
     return make_class

__test__ = dict(
    ex1 = """
>>> class Meta_A(type): pass
... 
>>> class Meta_B(type): pass
... 
>>> class A: __metaclass__ = Meta_A
... 
>>> class B: __metaclass__ = Meta_B
... 
>>> class C(A, B): pass
...
Traceback (most recent call last):
  File "<stdin>", line 1, in ?
TypeError: metaclass conflict: the metaclass of a derived class must be a (non-strict) subclass of the metaclasses of all its bases

>>> import __main__ as noconflict
>>> class Meta_C(Meta_A, Meta_B): pass
...
>>> class C(A, B): __metaclass__ = Meta_C
...

>>> class C(A, B): __metaclass__ = noconflict.classmaker()
...
>>> class D(A):
...     __metaclass__ = noconflict.classmaker((Meta_B,))
""",
    remove_redundant="""
    >>> class C1(object): pass
    ...
    
    >>> class C2(object): pass
    ...
    >>> from __main__ import remove_redundant
    >>> remove_redundant((C1, C2, C1))
    (<class '__main__.C1'>, <class '__main__.C2'>)
""")

if __name__ == "__main__":
    import doctest,__main__
    doctest.testmod(__main__)
