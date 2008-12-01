import os, sys, inspect
from tempfile import mkstemp

def _indent(body, prefix='    '):
    return '\n'.join(prefix + line for line in body.splitlines())
  
def _callermodule(level=2):
    return sys._getframe(level).f_globals.get('__name__', '?')
 
def getsignature(func):
    "Return the signature of a function as a string"
    argspec = inspect.getargspec(func)
    return inspect.formatargspec(formatvalue=lambda val: "", *argspec)[1:-1]

def makefn(name, signature, body, defaults=None, doc=None, module=None,
           funcdict=None, **evaldict):
    src = ('def %s(%s):\n' % (name, signature)) + _indent(body) + '\n'
    if makefn.save_source:
        fhandle, fname = mkstemp()
        os.write(fhandle, src)
        os.close(fhandle)
    else:
        fname = '?'
    code = compile(src, fname, 'single')
    exec code in evaldict
    func = evaldict[name]
    if defaults:
        func.func_defaults = defaults
    func.__doc__ = doc
    func.__dict__ = funcdict or {}
    func.__module__ = module or _callermodule() 
    func.__source__ = src
    return func

makefn.save_source = True

def decorator(caller, func=None):
    """
    decorator(caller) converts a caller function into a decorator;
    decorator(caller, func) is the same as decorator(caller)(func).
    """
    if func:
        signature = getsignature(func)
        for arg in signature.split(','):
            name = arg.strip(' *')
            assert not name in('_func_', '_call_'), (
               '%s is a reserved argument name!' % name)
        body = "_call_(_func_, %s)" % signature
        return makefn(func.__name__, signature, body, module=_callermodule(),
                      funcdict=func.__dict__, _func_=func, _call_=caller)
    return makefn(caller.__name__, 'func',
                  'return decorator(caller, func)',
                  module=_callermodule(), funcdict=caller.__dict__,
                  caller=caller, decorator=decorator)

import warnings

@decorator
def deprecated(func, *args, **kw):
    "A decorator to be applied to deprecated function"
    warnings.warn('Calling the deprecated function %r' % func.__name__,
                  DeprecationWarning, stacklevel=3)
    return func(*args, **kw)
