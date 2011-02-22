import re
reobj=re.compile(r'x')
from oopp import Str
sum=Str('a')+Str('b') # check the sum
print sum, type(sum)
rprod=Str('a')*2 # check the right product 
print rprod,type(rprod)
lprod=2*Str('a') # check the left product 
print lprod,type(lprod)
r=Str('a').replace('a','b') # check replace
print r,type(r)
r=Str('a').capitalize() # check capitalize
print r,type(r)
from oopp import *
aob=BaseRegexp('a')|BaseRegexp('b'); print aob
print pretty(attributes(aob))
from oopp import *
time_=ClsFactory[Traced](time)
print time_.asctime()
from oopp import *
class TracedTomatoPizza(GenericPizza,WithLogger):
    __metaclass__=ClsFactory[Traced] 
    toppinglist=['tomato']
marinara=TracedTomatoPizza('small') # nothing happens
import oopp
s='GenericPizza(object):'
oopp2=oopp.modulesub(s,s+'\n    __metaclass__=oopp.Traced',oopp) 
class PizzaLog(oopp2.CustomizablePizza,oopp2.WithLogger):
    __metaclass__=makecls()
marinara=PizzaLog.With(toppinglist=['tomato'])('small')
print type(type(PizzaLog)) #meta-metaclass
print oopp.MRO(PizzaLog)
print oopp.MRO(type(PizzaLog)) # the metaclass hierarchy
print oopp.MRO(type(type(PizzaLog))) # the meta-metaclass hierarchy
