# standard way

def decorator_trace(f):
    def newf(*args, **kw):
        print "calling %s with args %s, %s" % (f.__name__, args, kw)
        return f(*args, **kw)
    newf.__name__ = f.__name__
    newf.__dict__ = f.__dict__
    newf.__doc__ = f.__doc__
    return newf

@decorator_trace
def f1(x):
    pass


from decorator import decorator

def trace(f, *args, **kw):
    print "calling %s with args %s, %s" % (f.func_name, args, kw)
    return f(*args, **kw)

@decorator(trace)
def f1(x):
    pass

@decorator(trace)
def f(x, y=1, z=2, *args, **kw):
    pass

f(0, 3)

print getargspec(f) 
(['x', 'y', 'z'], 'args', 'kw', (1, 2))

@decorator(trace)
def exotic_signature((x, y)=(1,2)):
    return x+y

print getargspec(exotic_signature)

#################################################

import threading

def delayed(nsec):
    def call(proc, *args, **kw):
        thread = threading.Timer(nsec, proc, args, kw)
        thread.start()
        return thread
    return decorator(call)

@delayed(4)
def print_hello():
    print "hello"
    
print_hello()
