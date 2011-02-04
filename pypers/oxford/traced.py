# traced.py

def traced(func):
    def tracedfunc(*args, **kw):
        print "calling %s.%s" % (func.__module__, func.__name__)
        return func(*args, **kw)
    tracedfunc.__name__ = func.__name__
    return tracedfunc

@traced
def f(): pass


