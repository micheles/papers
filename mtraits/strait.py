__all__ = ['include', 'MetaTOS']

import inspect, types, warnings

class OverridingError(NameError):
    pass

class OverridingWarning(Warning):
    pass

class Super(object):
    # this is needed to fix a shortcoming of unbound super objects,
    # i.e. this is how the unbound version of super should work
    def __init__(self, thisclass):
        self.__thisclass__ = thisclass
    def __get__(self, obj, objcls):
        return super(self.__thisclass__, obj or objcls)

def oldstyle(*bases):
    "Return True if there are no bases or all bases are old-style"
    return not bases or set(map(type, bases)) == set([types.ClassType])

class Namespace(dict):
    "A named dictionary containing the attribute of a class and its ancestors"
    @classmethod
    def from_cls(klass, cls):
        if oldstyle(cls):
            mro = inspect.getmro(cls)
        else:
            mro = cls.__mro__[:-1] # all except object
        dic = merge(subc.__dict__ for subc in reversed(mro))
        return klass(cls.__name__, dic)
    def __init__(self, name, attrs):
        self.__name__ = name
        self.update(attrs)

def merge(dicts):
    """Merge a sequence of dictionaries. In case of name clashes, 
    the last dict in the sequence wins."""
    dic = {}
    for d in dicts:
        dic.update(d)
    return dic

class MetaTOS(type):
    "The metaclass of the Trait Object System"
    def __new__(mcl, name, bases, dic):
        if len(bases) > 1:
            raise TypeError(
                'Multiple inheritance of bases %s is forbidden for TOS classes'
                % str(bases))
        elif oldstyle(*bases): # converts into new-style
            bases += (object,)
        cls = mcl.__super.__new__(mcl, name, bases, dic)
        setattr(cls, '_%s__super' % name, Super(cls))
        return cls

MetaTOS._MetaTOS__super = Super(MetaTOS)

def find_common_names(namespaces):
    "Perform n*(n-1)/2 namespace overlapping checks on a set of n namespaces"
    n = len(namespaces)
    if n <= 1: return
    names = map(set, namespaces)
    for i in range(0, n):
        for j in range(i+1, n):
            ci, cj = namespaces[i], namespaces[j]
            common = names[i] & names[j]
            if common:
                yield common, ci, cj

def check_overridden(namespaces, exclude=frozenset(), raise_='error'):
    "Raise an OverridingError for common names not in the exclude set"
    for common, n1, n2 in find_common_names(namespaces):
        overridden = ', '.join(common - exclude)
        if overridden:
            msg = '%s overrides names in %s: {%s}' % (
                n1.__name__, n2.__name__, overridden)
            if raise_ == 'error':
                raise OverridingError(msg)
            elif raise_ == 'warning':
                warnings.warn(msg, OverridingWarning, stacklevel=2)
           
known_metas = set([MetaTOS])

def get_right_meta(metatos, bases):
    # there is only one base because of the single-inheritance constraint
    try:
        base = bases[0]
    except IndexError:
        base = object
    meta = type(base)
    if meta in (types.ClassType, type): # is a builtin meta
        return metatos
    elif any(issubclass(meta, m) for m in known_metas):
        return meta
    # meta is independent from all known_metas, make a new one with
    # __new__ method coming from MetaTOS
    newmeta = type(
        '_TOS' + meta.__name__, (meta,), dict(__new__=metatos.__new__))
    setattr(newmeta, '_%s__super' % metatos.__name__, Super(newmeta))
    known_metas.add(newmeta)
    return newmeta

exclude_attrs = set('__doc__ __module__ __dict__ __weakref__'.split())

def new(metatos, name, bases, attrs, traits):
    # traits as in Squeak take the precedence over the base class
    # but they are overridden by attributes in the class
    namespaces = map(Namespace.from_cls, traits)
    check_overridden(namespaces, exclude=set(attrs)|exclude_attrs)
    meta = get_right_meta(metatos, bases)
    cls = meta(name, bases, merge(namespaces + [Namespace(name, attrs)]))
    cls.__traits__ = traits
    for t in traits:
        setattr(cls, '_%s__super' % t.__name__, Super(cls))
    return cls

def include(*traits, **kw):
    "Returns a class factory"
    metatos = kw.get('MetaTOS', MetaTOS) # other kw free for future extensions
    def makecls(name, bases, dic):
        return new(metatos, name, bases, dic, traits)
    makecls.__name__ = 'include_%s' % '_'.join(m.__name__ for m in traits)
    return makecls
