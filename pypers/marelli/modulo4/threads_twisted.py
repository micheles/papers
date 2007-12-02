from twisted.internet.threads import deferToThread
from twisted.internet import reactor
from twisted.python import threadable; threadable.init(True)
import time

def print_(x):
    print x
    
@deferToThread.__get__
def do_nothing(err=False):
    if err: raise RuntimeError() 
    for i in range(10):
        #if not thread.running: break
        time.sleep(.5)
    return i

d1 = do_nothing()
d1.addBoth(print_)
d2 = do_nothing(err=False)
d2.addBoth(print_)

## print do_nothing
## d2 = do_nothing()

## d1.start()
## d2.start()
## print d1.getResult()
## d1.finish()
## time.sleep(1)
## d2.finish()
## print d1.isAlive()
## print d2.isAlive()
## print d1.getResult()
## print d2.getResult()

reactor.run()
