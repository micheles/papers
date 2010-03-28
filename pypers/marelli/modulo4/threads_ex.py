from ms.concurrency import ThreadFactory
import time

@ThreadFactory
def do_nothing(thread, err=False):
    if err: raise RuntimeError() 
    for i in range(10):
        if not thread.running: break
        time.sleep(.5)
        print thread.getName()
    return i

d1 = do_nothing(err=True)
print do_nothing
d2 = do_nothing()

d1.start()
d2.start()
print d1.getResult()
d1.finish()
time.sleep(1)
d2.finish()
print d1.isAlive()
print d2.isAlive()
print d1.getResult()
print d2.getResult()
