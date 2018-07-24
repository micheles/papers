class BaseWrapper(object):
    "Base class for interface wrappers built from mixin classes"
    def __init__(self, obj):
        self.__obj = obj
    def __get__(self, obj, objcls=None):
        if obj is None:
            return self
        return self.__class__(obj)
    def __getattr__(self, name):
        obj = self.__obj
        if obj is None:
            raise TypeError('Unbound wrapper %r' % self)
        return getattr(obj, name)
    def __repr__(self):
        names = ', '.join(n for n in dir(self) if not n.startswith('_'))
        msg = 'bound to %r' % self.__obj if self.__obj is not None else ''
        return '<%s {%s} %s>' % (self.__class__.__name__, names, msg)

def iwrapper(mixin):
    "Convert a mixin class into an interface wrapper object"
    # could be implemented with single-inheritance only, but why bother?
    cls = type(mixin.__name__ + 'Wrapper', (BaseWrapper, mixin), {})
    return cls(None)
