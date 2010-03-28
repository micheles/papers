import threading
from decorators import decorator

def deferred(nsec):
    def inner_deferred(func, *args, **kw):
        return threading.Timer(nsec, func, args, kw).start()
    return decorator(inner_deferred)

    
@deferred(2)
def hello():
    print "hello"

print "Calling hello ..."
hello()
