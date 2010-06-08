from Tkinter import *
from ms.async_utils import TkinterLoopingCall, multi_iter
import itertools

nline = list(enumerate(file(__file__)))

def print_a(i):
    for n, line in nline:
        if i % 100 == 0:
            print "serving client #%6d, sending line #%s" % (i, n+1)
        yield None
    
def print_t(i):
    for n, line in nline:
        if i % 10 == 0 and n % 100 == 0:
            print "serving client #%6d, sending line #%s" % (i, n+1)
    
def test_a(N=1000*1000):
    print "Have patience ..."
    root = Tk()
    mi = multi_iter([print_a(i) for i in xrange(1, N+1)], terminate=False)
    lc = TkinterLoopingCall(mi.next)
    lc.start()
    root.after(10*60*1000, root.quit) # 10 minutes
    root.mainloop()



















import threading

def test_t(N=1000*1000):
    global nline
    from ms.debug_utils import killme; killme()
    nline = enumerate(itertools.count(1))
    for i in xrange(1, N+1):
        t = threading.Thread(target=print_t, args=(i,))
        t.start(); print t
    
if __name__ == "__main__":
    test_a()
