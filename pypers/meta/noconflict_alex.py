# noconflict.py
"""Deep, **DEEP** magic to remove metaclass conflicts.

``noconflict`` provides the ``safetype`` metaclass, the mother of conflict-free
metaclasses. The suggested import syntax for usage in other modules is

    from safetype import safetype as type

If you override ``__new__`` when you derive from ``safetype``,
you should do it cooperatively."""

import inspect, types, __builtin__
from ms.iter_utils import skip_redundant
try: set # python version >= 2.4
except NameError: # python version <= 2.3
   from sets import Set as set

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
    """Overrides the ``__new__`` method of the ``type`` metaclass, making the
    generation of classes conflict-proof."""
    def __new__(mcl, *args):
        nargs = len(args)
        if nargs == 1: # works as __builtin__.type
            return __type__(args[0]) 
        elif nargs == 3: # creates the class using the appropriate metaclass
            n, b, d = args # name, bases and dictionary
            meta = get_noconflict_metaclass(b, (mcl,), right_metas=()) 
            if meta is mcl: # meta is trivial, dispatch to the default __new__
                return super(safetype, mcl).__new__(mcl, n, b, d)
            else: # non-trivial metaclass, dispatch to the right __new__
                # (it will take a second round) 
                return super(mcl, meta).__new__(meta, n, b, d)
        else:
            raise TypeError('%s() takes 1 or 3 arguments' % mcl.__name__)
