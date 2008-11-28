from decorator import __call__
    
class Memoize(object):

    registry = {}
    
    @classmethod
    def clear(cls, cacheclass=object):
        for func in cls.registry:
            if issubclass(func.cacheclass, cacheclass):
                func.cache.clear()
    
    def __init__(self, cacheclass):
        self.cacheclass = cacheclass
        
    def call(self, func, *args, **kw):
        try:
            return func.cache[args]
        except KeyError:
            res = func.cache[args] = self.func(*args, **kw)
            return res

    def __call__(self, func):
        new = __call__(self, func)
        new.cache = {}
        new.cacheclass = self.cacheclass
        self.registry.append(new)
        return new

memoize = Memoize(object)

if __name__ == '__main__': # test
    
    @memoize
    def f():
        return 1
