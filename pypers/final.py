from oopp import *
class Final(Singleton,type):
    "Inherits the 'instance' attribute from Singleton (default None)"
    def __new__(meta,name,bases,dic):
        if meta.counter==0: # first call
            return super(Final,meta).__new__(meta,name,bases,dic)
        else:
            raise NonDerivableError("I cannot derive from %s" % bases)
  
class C:  __metaclass__=Final
try:
    class D(C): pass
except NonDerivableError,e:
    print e

