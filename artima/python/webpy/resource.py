import web

class inputmethod(object):
    """
    A descriptor passing web.input() as first argument to the
    inner function.
    """
    def __init__(self, func):
        self._func = func
    def __get__(self, obj, objcls):
        return self._func.__get__(web.input())

def identity(func):
    "A do nothing decorator"
    return func

class Register(object):
    """
    Each instance is a decorator associated to a web.py application.
    Each application adds a RESTful resource.
    """
    def __init__(self, dec=identity):
        self.dec = dec
        self.app = web.application([]) # self.app.mapping is the empty list
        self.mapping = {}
    
    def __call__(self, path):
        def dec(func):
            chunks = func.__name__.split('_')
            name = '_'.join(chunks[:-1])
            meth = chunks[-1]
            assert meth in ('GET', 'POST', 'PUT', 'DELETE'), meth
            func = self.dec(func) # decorate the original function
            self._addcls(path, name, meth, inputmethod(func))
            return func
        return dec

    def _addcls(self, path, name, method, meth):
        if path not in self.mapping:
            cls = type(name, (), dict(path=path))
            self.mapping[path] = cls
            self.app.mapping.extend([path, cls])
        else:
            cls = self.mapping[path]
        setattr(cls, method, meth)

if __name__ == '__main__':

    resource = Register()
    
    @resource('/b')
    def book_GET():
        pass

    @resource('/c')
    def book_POST():
        pass

    @resource('/d')
    def book_PUT():
        pass

    @resource('/e')
    def book_DELETE():
        pass

    print resource.app.mapping, resource.mapping
