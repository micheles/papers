"""
The namedtuple class here is inspired from the namedtuple class
used in Python 2.6. Unfortunately, I cannot use that class,
since it does not work for field names which are Python keywords, and names
coming from a database are uncontrollable. There is still an issue
for names starting and ending with a double underscore, and for the
following names: _fields, _fromdict and _asdict.
Conflict with such names however is pretty rare and it is ignored.
"""

from operator import itemgetter

RESERVED_NAMES = set('_asdict _fields _fromdict'.split())

class Namedtuple(tuple):
    _fields = ()
    __slots__ = ()

    @classmethod
    def _fromdict(cls, dic):
        return cls(dic[name] for name in self._fields)

    def _asdict(self):
        return dict(zip(self._fields, self))

    def __repr__(self):
        s = ', '.join('%r: %r' % (n, v) for (n, v) in zip(self._fields, self))
        return '%s(%s)' % (self.__class__.__name__, s)

def namedtuple(typename, fields):
    "Return a Namedtuple subclass without slots and with the right properties"
    dic = dict(_fields=fields, __slots__=())
    for i, f in enumerate(fields):
        if f.startswith('__') and f.endswith('__') or f in RESERVED_NAMES:
            raise NameError('%s is not a valid field name' % f)
        dic[f] = property(
            itemgetter(i), doc="Property for the field #%d, %r" % (i, f))
    return type(typename, (Namedtuple,), dic)
