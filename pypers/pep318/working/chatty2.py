# chatty2.py

import customdec; customdec.enhance_classes("[Decorated]")

# sets the log files
log1=file('file1.log','w')
log2=file('file2.log','w')

class C:
    def f(self): 
        "[chattymethod2]"
    f.logfile=log1 # function attribute
    def g(self): 
        "[chattymethod2]"
    g.logfile=log2 # function attribute

assert C.__dict__['f'].logfile is log1 # check the conversion 
assert C.__dict__['g'].logfile is log2 # function attr. -> decorator attr.

c=C() # C instantiation

c.f() # print a message in file1.log
c.g() # print a message in file2.log

log1.close(); log2.close() # finally


