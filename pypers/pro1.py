"Incredible behaviour of metafunctions"

from oopp import *

def magicallyTransform(name,bases,dic):
    print "called!"
    dic['formatstring']="Very beautiful, since I am %s"
    return type(name,bases,dic)
    #return TransformedUglyDuckling(name,bases,dic)

class MagicallyTransformed(Class):
    "Metaclass changing the formatstring of its instances"
    def __init__(cls,name,bases,dic):
        print "called!"
        cls.formatstring="Very beautiful, since I am %s"
          
class TransformedUglyDuckling(PrettyPrint):
    "A class metamagically modified"
    __metaclass__ = MagicallyTransformed
    __metaclass__ = magicallyTransform
    formatstring="Not beautiful, I am %s" # will be changed

class Swan(TransformedUglyDuckling): 
    formatstring="Very beautiful, I am %s"

print Swan()

print  Swan.__class__,Swan.__metaclass__
