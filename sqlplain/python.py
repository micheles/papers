import os, sys, re, inspect, warnings
from tempfile import mkstemp

PYTHON3 = sys.version >= '3'

DEF = re.compile('\s*def\s*([_\w][_\w\d]*)\s*\(')

def _callermodule(level=2):
    return sys._getframe(level).f_globals.get('__name__', '?')
 
def getsignature(func):
    "Return the signature of a function as a string"
    argspec = inspect.getargspec(func)
    return inspect.formatargspec(formatvalue=lambda val: "", *argspec)[1:-1]

class FuncData(object):
    def __init__(self, func=None, name=None, signature=None,
                 defaults=None, doc=None, module=None, funcdict=None):
        if func:
            self.name = func.__name__
            self.signature = getsignature(func)
            self.defaults = func.func_defaults
            self.doc = func.__doc__
            self.module = func.__module__
            self.dict = func.__dict__
        if name:
            self.name = name
        if signature:
            self.signature = signature
        if defaults:
            self.defaults = defaults
        if doc:
            self.doc = doc
        if module:
            self.module = module
        if funcdict:
            self.dict = funcdict

    def update(self, func, **kw):
        func.__name__ = getattr(self, 'name', 'noname')
        func.__doc__ = getattr(self, 'doc', None)
        func.__dict__ = getattr(self, 'dict', {})
        func.func_defaults = getattr(self, 'defaults', None)
        func.__module__ = getattr(self, 'module', _callermodule())
        func.__dict__.update(kw)
        return func
    
    def __getitem__(self, name):
        return getattr(self, name)
    
def makefn(src, funcdata, save_source=True, **evaldict):
    src += os.linesep # add a newline just for safety
    name = DEF.match(src).group(1) # extract the function name from the source
    if save_source:
        fhandle, fname = mkstemp()
        os.write(fhandle, src)
        os.close(fhandle)
    else:
        fname = '?'
    code = compile(src, fname, 'single')
    exec code in evaldict
    func = evaldict[name]
    return funcdata.update(func, __source__=src)

def decorator(caller, func=None):
    """
    decorator(caller) converts a caller function into a decorator;
    decorator(caller, func) is akin to decorator(caller)(func).
    """
    if func:
        fd = FuncData(func)
        name = fd.name
        signature = fd.signature
        for arg in signature.split(','):
            argname = arg.strip(' *')
            assert not argname in('_func_', '_call_'), (
               '%s is a reserved argument name!' % argname)
        src = """def %(name)s(%(signature)s):
        return _call_(_func_, %(signature)s)""" % locals()
        return makefn(src, fd, save_source=False, _func_=func, _call_=caller)
    src = 'def %s(func): return decorator(caller, func)' % caller.__name__
    return makefn(src, FuncData(caller), save_source=False,
                  caller=caller, decorator=decorator)

@decorator
def deprecated(func, *args, **kw):
    "A decorator for deprecated functions"
    warnings.warn('Calling the deprecated function %r' % func.__name__,
                  DeprecationWarning, stacklevel=3)
    return func(*args, **kw)

def upgrade_dec(dec):
    def new_dec(func):
        fd = FuncData(func)
        src = '''def %(name)s(%(signature)s):
        return decorated(%(signature)s)''' % fd
        return makefn(src, fd, save_source=False, decorated=dec(func))
    return FuncData(dec).update(new_dec)
