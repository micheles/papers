from decorator import __call__
    
class Memoize(object):

    fullregistry = {}
    
    def __init__(self, cacheclass):
        self.cacheclass = cacheclass
        self.fullregistry.setdefault(self.cacheclass, [])
        
    def call(self, func, *args, **kw):
        try:
            return func.cache[args]
        except KeyError:
            res = func.cache[args] = self.func(*args, **kw)
            return res

    def __call__(self, func):
        new = __call__(self, func)
        new.cache = {}
        self.registry.append(new)
        return new
    
    @property
    def registry(self):
        return self.fullregistry[self.cacheclass]

    @classmethod
    def clear(cls, cacheclass=None):
        if cacheclass is None:
            cls.fullregistry = {}; return
        for cc, functions in cls.fullregistry.items():
            if issubclass(cc, cacheclass):
                for func in functions:
                    func.cache.clear()

if __name__ == '__main__': # test
    
    @Memoize(object)
    def f():
        return 1
