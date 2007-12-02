from oopp import Final
class C:
   __metaclass__=Final
from oopp import singletonClass
C=singletonClass()
class D(C):
   pass
id(C),id(D)
C is D
type(C)
type(C).__bases__
c=C(); d=D()
id(c),id(d)
