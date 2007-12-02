# traced_function2.py

from decorators import decorator

def trace(f, *args, **kw):
    print "calling %s with args %s, %s" % (f.func_name, args, kw) 
    return f(*args, **kw)

traced_function = decorator(trace)

@traced_function
def f1(x):
    pass

@traced_function
def f2(x, y):
    pass


