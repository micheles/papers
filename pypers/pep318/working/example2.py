# example2.py

from decorators import decorated
from example1 import do_nothing,identity,name

class B(object):
    "This is a regular class"

B=decorated(B) # does nothing

class C(B):
   "[Decorated]"
   do_nothing=do_nothing
   identity=identity
   name=name

C=decorated(C) # regenerates the class converting methods in decorators
c=C()



class D: # old style
    "[Decorated]"
    def identity(x):
        "[staticmethod]"
        return x

D=decorated(D)

d=D()

# test
assert d.identity(1) == 1
assert D.identity(1) == 1 


