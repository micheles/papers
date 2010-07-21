from customdec import *

def f(self): pass

tracedf=tracedmethod(f)
tracedtracedf=decorated(tracedmethod(f))

class C: pass

c=C()

C.f=tracedtracedf

c.f()

class Chatty(ClassDecorator):
    def __init__(cls,*args):
        print 'Chatty.__init__'

class C:pass



C=Chatty(Chatty(C))
