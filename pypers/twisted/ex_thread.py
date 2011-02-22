from twisted.internet import reactor
from twisted.internet.threads import deferToThread
import time, sys

@deferToThread.__get__    
def longrunning(proc):
    for i in range(100):
        time.sleep(.1)
        sys.stdout.write(proc)
        sys.stdout.flush()
    return "ended %r" % proc

def print_(arg):
    print arg

longrunning("+").addBoth(print_)
longrunning("-").addBoth(print_)
longrunning("*").addBoth(print_)
longrunning("/").addBoth(print_)
reactor.run()
