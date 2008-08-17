import re, sys, inspect, types, warnings

class OverridingError(NameError):
    pass

class OverridingWarning(Warning):
    pass

def getnames(obj):
    "Get the nonspecial attributes in obj"
    return set(name for name in dir(obj)
               if not (name.startswith('__') and name.endswith('__')))

def find_common_names(mixins):
    "Perform n*(n-1)/2 namespace overlapping checks on a set of n mixins"
    n = len(mixins)
    if n <= 1: return
    names = [set(getnames(obj)) for obj in mixins]
    for i in range(0, n):
        for j in range(i+1, n):
            ci, cj = mixins[i], mixins[j]
            common = names[i] & names[j]
            if common:
                yield common, ci, cj

def check_overridden(mixins, exclude, raise_='error'):
    "Raise an OverridingError for common names not in the exclude set"
    for common, c1, c2 in find_common_names(mixins):
        overridden = ', '.join(common - exclude)
        if overridden:
            msg = '%s overrides names in %s: {%s}' % (
                c1.__name__, c2.__name__, overridden)
            if raise_ == 'error':
                raise OverridingError(msg)
            elif raise_ == 'warning':
                warnings.warn(msg, OverridingWarning, stacklevel=2)

def getboundvalue(value, obj, objcls):
    "Convert a value into a bound descriptor or do nothing"
    try: # return the bound descriptor
        return value.__get__(obj, objcls)
    except AttributeError: # not a descriptor
        return value

# added to the instances of TOSMeta
def __obj_getattribute__(obj, name, get=object.__getattribute__):
    """
    Lookup for TOS instances:
    1. look at the instance dictionary;
    2. look at the class dictionary;
    3. look at the traits;
    4. look at the base classes and to __getattr__
    """
    if name.startswith('__') and name.endswith('__'): # special name, do nothing
        return get(obj, name)
    try:
        return vars(obj)[name]
    except KeyError:
        pass
    objcls = type(obj)
    try:
        return getboundvalue(vars(objcls)[name], obj, objcls)
    except KeyError:
        pass
    for boundtrait in obj.__traits__:
        try:
            return getattr(boundtrait, name)
        except AttributeError:
            pass
    return get(obj, name)

# added to TOSMeta
def __cls_getattribute__(cls, name, get=type.__getattribute__):
    """
    Lookup for TOS classes:
    1. look at the class dictionary;
    2. look at the traits;
    3. look at the base classes and the metaclass __getattr__
    """
    if (name.startswith('__') and name.endswith('__')) or name == 'mro': 
        # special names, do nothing
        return get(cls, name)
    try:
        return getboundvalue(vars(cls)[name], None, cls)
    except KeyError:
        pass
    for boundtrait in cls.__traits__:
        try:
            return getattr(boundtrait, name)
        except AttributeError:
            pass
    return get(cls, name)

class Trait(object):
    """
    Class for mixin dispatchers. Mixin dispatchers are instantiated through the 
    .to classmethod and possess a __mixin__ attribute. They are descriptors 
    acting as proxies to an inner dictionary.
    There are bound and unbound dispatchers, just as there are methods and 
    functions. A bound dispatcher is a dispatcher instance bound to a class or 
    an object, whereas Trait(mixin(), name) returns an unbound dispatcher.
    """

    def __init__(self, inner, name, obj=None, objcls=None):
        if isinstance(inner, self.__class__): # already a trait
            self.__inner = inner._Trait__inner
        else:
            self.__inner = inner
        self.__name__ = name
        self.__obj = obj
        self.__objcls = objcls

    def __get__(self, obj, objcls=None): 
        "Return a bound dispatcher"
        return self.__class__(self.__inner, self.__name__, obj, objcls)

    def __getattr__(self, name):
        "obj.dispatcher.method(args) returns mixin.method(obj, args)"
        value = getattr(self.__inner, name)
        try: # if (unbound) method, go back to the function
            value = value.im_func
        except AttributeError:
            pass
        obj, objcls = self.__obj, self.__objcls
        if obj or objcls:
            try: # return the bound descriptor
                return value.__get__(obj, objcls)
            except AttributeError: # not a descriptor
                pass
        return value

    def __iter__(self):
        return iter(getnames(self.__inner))

    def __repr__(self):
        names = ', '.join(sorted(self))
        bound_obj = self.__obj or self.__objcls
        if bound_obj:
            msg = 'bound to %r' % bound_obj
        else:
            msg = ''
        return '<%s %s {%s} %s>' % (
            self.__class__.__name__, self.__name__, names, msg)

    ## we could live with nonpickeable traits, but still ...

    def __getstate__(self):
        return self.__inner

    def __setstate__(self, inner):
        self.__init__(inner, inner.__name__)

class TraitContainer(object):

    @classmethod
    def from_(cls, mixins):
        return cls(dict((m.__name__, Trait(m, m.__name__)) for m in mixins))

    def __init__(self, dic, obj=None, objcls=None):
        self.__traits = dic # a dictionary name -> trait
        self.__obj = obj
        self.__objcls = objcls
        
    def __getattr__(self, name):
        try:
            trait = self.__traits[name]
        except KeyError:
            raise AttributeError(name)
        else:
            return trait.__get__(self.__obj, self.__objcls)

    def __iter__(self):
        return (t.__get__(self.__obj, self.__objcls) 
                for t in self.__traits.itervalues())

    def __len__(self):
        return len(self.__traits)

    def __bool__(self):
        return bool(self.__traits)

    def __get__(self, obj, objcls=None):
        return self.__class__(self.__traits, obj, objcls)

    def __repr__(self):
        bound_obj = self.__obj or self.__objcls
        if bound_obj:
            msg = 'bound to %r' % bound_obj
        else:
            msg = ''
        return '<Traits %s %s>' % (', '.join(self.__traits), msg)

    def __getstate__(self):
        return self.__traits

    def __setstate__(self, dic):
        self.__init__(dic)

def oldstyle(bases):
    "Return True if there are no bases or all bases are old-style"
    return not bases or set(map(type, bases)) == set([types.ClassType])

def isTOSclass(cls):
    "True if cls satisfies the TOS interface"
    return hasattr(cls, '__traits__')

class TOSMeta(type):
    """
    The metaclass for the Trait Object System. It is intended to be
    called only indirectly via ``include``. It provides the following features
    to its instances:
    1. forbids multiple inheritance
    2. checks for accidental overriding of __getattribute__ and __getstate__
    3. provides the class with the correct base __getattribute__ and 
       __getstate__
    4. provides the basic empty __traits__ attribute and __mixins__.
    """
    def __new__(mcl, name, bases, dic):
        dic = dic.copy()
        if len(bases) > 1:
            raise TypeError(
                'Multiple inheritance of bases %s is forbidden for TOS classes'
                % str(bases))
        elif oldstyle(bases): # ensure new-style class
            bases += (object, )
        for meth in ('__getattribute__', '__getstate__'):
            if meth in dic:
                raise OverridingError('class %s defines %s' % (name, meth))
        traits = getattr(bases[0], '__traits__', ())
        if not traits: # the first time
            dic['__getattribute__'] = dic.get('__getattribute__', 
                                              __obj_getattribute__)
            dic['__getstate__'] = dic.get('__getstate__', vars)
            basemixins = ()
        else:
            basemixins = tuple(t._Trait__inner for t in traits)
        mixins = dic.get('__mixins__', ())
        if mixins:
            commonset = set(basemixins) & set(mixins)
            if commonset:
                raise TypeError("Redundant mixins %s!", commonset)
            mixins = basemixins + mixins
            check_overridden(mixins, exclude=set(dic))
            dic['__traits__'] = TraitContainer.from_(mixins)
        # since TOS hierarchies are single-inheritance, I don't need super
        return mcl.__base__.__new__(mcl, name, bases, dic)

    __getattribute__ = __cls_getattribute__

known_metas = set([types.ClassType, type, TOSMeta])

def new(mcl, name, bases, dic, mixins):
    "Returns a class akin to objcls, but meta-enhanced with mcl or typ"
    # there is only one base because of the single-inheritance constraint
    try:
        base = bases[0]
    except IndexError:
        base = object
    typ = mcl or type(base)
    if typ in (types.ClassType, type):
        typ = TOSMeta
    elif typ not in known_metas:
        typ = type('_TOSMeta' + typ.__name__, (mcl,), dict(
            __new__=TOSMeta.__new__, __getattribute__= __cls_getattribute__))
        known_metas.add(typ)
    dic['__mixins__'] = mixins
    return typ(name, bases, dic)

def include(*mixins):
    "Class decorator factory"
    frame = sys._getframe(1)
    if ('__module__' in frame.f_locals and not # we are in a class
        '__module__' in frame.f_code.co_varnames):
        # usage as a Python < 2.6 class decorator
        mcl = frame.f_locals.get("__metaclass__")
        def makecls(name, bases, dic):
            return new(mcl, name, bases, dic, mixins)
        frame.f_locals["__metaclass__"] = makecls
    else:
        # usage as a Python >= 2.6 class decorator
        def _include(cls):
            return new(cls.__class__, cls.__name__, cls.__bases__, 
                       cls.__dict__.copy(), mixins)
        _include.__name__ = 'include_%s>' % '_'.join(m.__name__ for m in mixins)
        return _include
