# chatty.py

from customdec import decorated,chattymethod
chattymethod.logfile=file('file1.log','w')

class C(object):
    "[Decorated]"
    def f():
        "[chattymethod, staticmethod]"

C=decorated(C)
c=C()

c.f()
C.f()



chattymethod.logfile=file('file2.log','w')

def g(): 
    "[chattymethod,staticmethod]"

C.g=decorated(g)
C.g # message written in file2.log
C.f # message written in file2.log


