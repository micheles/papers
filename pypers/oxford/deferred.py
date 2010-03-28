# deferred.py
"Deferring the execution of a procedure (function returning None)"

import threading
from decorators import decorator

def deferred(nsec):
    def call_later(func, *args, **kw):
        return threading.Timer(nsec, func, args, kw).start()
    return decorator(call_later)

@deferred(2)
def hello():
    print "hello"

if __name__ == "__main__":
    hello()    
    print "Calling hello() ..."
   


