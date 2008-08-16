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

def __getattr__(obj, name):
    "__getattr__ added by TOSMeta"
    objcls = obj.__class__
    for trait in obj.__traits__:
        bt = trait.__get__(obj, objcls) # bound trait
        try:
            return getattr(bt, name)
        except AttributeError:
            continue
    raise AttributeError(name)

class TOSMeta(type):
    """The metaclass for the Trait Object System. It is intended to be
    called only indirectly via ``include``. It provides the following features
    to its instances:
    1. forbids multiple inheritance
    2. checks for accidental overriding of __getattr__ and __getstate__
    3. provides the class with the correct base __getattr__ and __getstate__
    4. provides the basic empty __traits__ attribute
    """

    def __new__(mcl, name, bases, dic):
        if len(bases) > 1:
            raise TypeError(
                'Multiple inheritance of bases %s is forbidden for TOS classes'
                % str(bases))
        elif oldstyle(bases): # ensure new-style class
            bases += (object, )
        for meth in ('__getattr__', '__getstate__'):
            if meth in dic:
                raise OverridingError('class %s defines %s' % (name, meth))
        traits = getattr(bases[0], '__traits__', ())
        if not traits: # the first time
            dic['__getattr__'] = dic.get('__getattr__', __getattr__)
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
        return super(TOSMeta, mcl).__new__(mcl, name, bases, dic)

    def __getattr__(cls, name):
        for trait in cls.__traits__:
            bt = trait.__get__(None, cls) # class bound trait
            try:
                return getattr(bt, name)
            except AttributeError:
                continue
        raise AttributeError(name)

cache = {type: TOSMeta, types.ClassType: TOSMeta} # metaclass cache

def getrightmeta(metacls, mcl):
    "Determine the right metaclass to use between metacls and mcl (=TOSMeta)"
    if issubclass(metacls, mcl): # has TOSMeta functionality
        return metacls
    else: # add TOSMeta functionality
        try:
            return cache[metacls]
        except KeyError:
            cache[metacls] = type('TOS' + metacls.__name__, (mcl, metacls), {})
            return cache[metacls]

def new(mcl, objcls, mixins):
    "Returns a class akin to objcls, but meta-enhanced with mcl (TOSMeta)"
    dic = vars(objcls).copy()
    dic['__mixins__'] = mixins
    metacls = getrightmeta(type(objcls), mcl)
    return metacls(objcls.__name__, objcls.__bases__, dic)

def oldstyle(bases):
    "Return True if there are not bases or all bases are old-style"
    return not bases or set(map(type, bases)) == set([types.ClassType])

def include(*mixins):
    "Class decorator factory"
    frame = sys._getframe(1)
    if ('__module__' in frame.f_locals and not # we are in a class
        '__module__' in frame.f_code.co_varnames):
        # usage as a Python < 2.6 class decorator
        mcl = frame.f_locals.get("__metaclass__")
        def makecls(name, bases, dic):
            if mcl:
                cls = mcl(name, bases, dic)
            elif oldstyle(bases):
                cls = types.ClassType(name, bases, dic)
            else: # there is at least a new style base
                cls = type(name, bases, dic)
            return new(TOSMeta, cls, mixins)
        frame.f_locals["__metaclass__"] = makecls
    else:
        # usage as a Python >= 2.6 class decorator
        def _include(cls):
            return new(TOSMeta, cls, mixins)
        _include.__name__ = 'include_%s>' % '_'.join(m.__name__ for m in mixins)
        return _include

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
        return self.__traits.itervalues()

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

def get_traits(obj):
    "Returns a container of traits for introspection purposes"
    return getattr(obj, '__traits__', ())

class Super(object):
    """
    A simple implementation of the super object valid for single inheritance
    hierarchies.
    """
    def __init__(self, obj=None, cls=None):
        assert obj or cls, 'Super objects must be bound to something'
        self.__obj = obj
        self.__objcls = cls or obj.__class__
    
    def __getattribute__(self, name, get=object.__getattribute__):
        obj, objcls = get(self, '_Super__obj'), get(self, '_Super__objcls')
        attr = getattr(objcls.__bases__[0], name)
        try: # return the bound descriptor
            return attr.__get__(obj, objcls)
        except AttributeError: # not a descriptor
            return attr

import readline, rlcompleter, re

try:
    from IPython.completer import Completer as BaseCompleter
except ImportError:
    from rlcompleter import Completer as BaseCompleter

class Completer(BaseCompleter):
    def attr_matches(self, text):
        m = re.match(r"(\w+(\.\w+)*)\.(\w*)", text)
        if not m:
            return
        expr, attr = m.group(1, 3)
        object = eval(expr, self.namespace)
        words = dir(object)
        if hasattr(object,'__class__'):
            words.append('__class__')
            words += rlcompleter.get_class_members(object.__class__)
        if hasattr(object, '_Trait__dic'):
            words += object._Trait__dic.keys()
        matches = []
        n = len(attr)
        for word in words:
            if word[:n] == attr and word != "__builtins__":
                matches.append("%s.%s" % (expr, word))
        return matches

readline.set_completer(Completer().complete)
readline.parse_and_bind("TAB: complete")
