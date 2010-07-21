# metatracer.py

import inspect
from decorators import decorator

@decorator
def traced(meth, *args, **kw):
    cls = meth.__cls__
    modname = meth.__module__ or cls.__module__
    print "calling %s.%s.%s" % (modname, cls.__name__, meth.__name__)
    return meth(*args, **kw)

class MetaTracer(type):            
    def __init__(cls, name, bases, dic):
        super(MetaTracer, cls).__init__(name, bases, dic)
        for k, v in dic.iteritems():
            if inspect.isfunction(v):
                v.__cls__ = cls # so we know in which class v was defined
                setattr(cls, k, traced(v))


