
from ms.lang_utils import WrapMethods, decorator

# to be called from the metaclass
@decorator
def tracer(func, *args, **kw):
    cls = func.__cls__
    modname = func.__module__ or cls.__module__
    print "calling %s.%s.%s" % (modname, cls.__name__, func.__name__)
    return func(*args, **kw)

class Traced(tracer.metaclass):
    def __init__(cls, name, bases, dic):
        for name, attr in dic.iteritems():
            if not name.startswith("__"):
                setattr(cls, name, attr)
        
print tracer.metaclass

class C:
    __metaclass__ = Traced
    def __init__(self):
        pass
    def f(self):
        pass
    
c = C()
c.f()
