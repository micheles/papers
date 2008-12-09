from decorator import decorator

class Memoize(object):

    registry = []
    
    @classmethod
    def clear(cls, cachetype=object):
        for func in cls.registry:
            if issubclass(func.cachetype, cachetype):
                func.cache.clear()
    
    def __init__(self, cachetype):
        self.cachetype = cachetype
        
    def call(self, func, *args, **kw):
        key = args, frozenset(kw.iteritems())
        try:
            return func.cache[key]
        except KeyError:
            res = func.cache[key] = func(*args, **kw)
            return res

    def __call__(self, func):
        func.cache = {}
        new = decorator(self.call, func)
        new.cachetype = self.cachetype
        self.registry.append(new)
        return new

memoize = Memoize(object)

if __name__ == '__main__': # test
    
    @memoize
    def f():
        return 1
