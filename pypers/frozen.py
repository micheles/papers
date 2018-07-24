from oopp import *
class C(Frozen):
    c=1
    def __init__(self): 
        #self.x=5 # won't work anymore, __new__ will be okay
        pass
class D(C):
    d=2
  
C.c=2
print D().d
