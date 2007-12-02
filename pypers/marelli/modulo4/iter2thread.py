import time
from ms.concurrency import gen2thread

@gen2thread
def genNumbers(self, N):
    for i in range(N):
        time.sleep(.2)
        print i
        yield None


t = genNumbers(10)
t.start()
time.sleep(1)
t.stop()
