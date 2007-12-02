# example7.py

from example2 import identity,name
import inspect, decorators; decorators.enhance_classes("[Decorated]")

class C1(object): # automagically converted to a decorated class
    identity=identity 

class C2: # automagically converted to a decorated class
    name=name

c1=C1() # C1 instance
c2=C2() # C2 instance


