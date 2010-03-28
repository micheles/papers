"""MagicSuper: an example of metaclass recompiling the source code.
This provides Python with a ``callsupermethod`` macro simplifying
the cooperative call syntax.
Examples:

from magicsuper import object

class B(object):
    def __new__(cls, *args, **kw):
        print "B.__new__"
        return callsupermethod(cls)
    def __init__(self, *args, **kw):
        print "B.__init__"
        callsupermethod(*args, **kw)
    @staticmethod
    def sm():
        print "B.sm"
    @classmethod
    def cm(cls):
        print cls.__name__
    
    
class C(B):
    def __new__(cls, *args, **kw):
        print args, kw
        return callsupermethod(cls, *args, **kw)
    @staticmethod
    def sm():
        callsupermethod()
    @classmethod
    def cm(cls):
        callsupermethod()
    
c = C()
c.cm()
c.sm()
"""

import inspect, textwrap
        
class MagicSuper(type):
    def __init__(cls, clsname, bases, dic):
        clsmodule = __import__(cls.__module__) #assume cls is defined in source
        for name, value in dic.iteritems():
            # __new__ is seen as a function in the dic, so it has
            # to be converted explicitly into a staticmethod;
            # ordinary staticmethods don't type-dispatch on their
            # first argument, so use 'super(cls, cls)' for them.
            was_staticmethod = False
            if isinstance(value, staticmethod):
                value = value.__get__("dummy") # convert to function
                was_staticmethod = True
            elif isinstance(value, classmethod):
                value = value.__get__("dummy").im_func # convert to function
            if inspect.isfunction(value):
                if was_staticmethod:
                    first_arg = clsname
                else:
                    first_arg = inspect.getargspec(value)[0][0]
                source = textwrap.dedent(inspect.getsource(value))
                if not 'callsupermethod' in source: continue
                source = source.replace(
                    'callsupermethod', 'super(%s, %s).%s'
                    % (clsname, first_arg, name))
                #print source # debug
                exec source in clsmodule.__dict__, dic # modifies dic
                if name == "__new__":
                    dic[name] = staticmethod(dic[name])
                setattr(cls, name, dic[name])

object = MagicSuper("object", (), {})
type = MagicSuper("type", (type,), {})

# example:

class B(object):
    def __new__(cls, *args, **kw):
        return callsupermethod(cls)
    def __init__(self, *args, **kw):
        print "B.__init__"
        callsupermethod(*args, **kw)
    @staticmethod
    def sm():
        print "B.sm"
    @classmethod
    def cm(cls):
        print cls.__name__
    
    
class C(B):
    def __new__(cls, *args, **kw):
        print args, kw
        return callsupermethod(cls, *args, **kw)
    @staticmethod
    def sm():
        callsupermethod()
    @classmethod
    def cm(cls):
        callsupermethod()
    
if __name__ == "__main__":
    c = C(1, x=2)
    c.sm()
    c.cm()
