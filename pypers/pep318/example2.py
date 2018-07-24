# example2.py

from decorators import decorator
from example1 import do_nothing,identity,name

class B(object):
    "This is a regular class"

B=decorator(B) or B # return the original B

class C(B):
   "[Decorated]"
   do_nothing=do_nothing
   identity=identity
   class Inner: # old style class
       "[Decorated]" # required docstring   
       name=name

C=decorator(C) # regenerate the class converting methods in decorators
c=C()


