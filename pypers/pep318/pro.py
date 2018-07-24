"""Funziona solo se e' composto una sola volta, senno' ricorsione
infinita""" # ??

import customdec,__builtin__
__builtin__.type=customdec.type
__builtin__.object=customdec.TraceFunctions(object)
print __builtin__.object.__mro__ 

__builtin__.object=customdec.Decorated(customdec.TraceFunctions(object))
print __builtin__.object.__mro__ 

#raise SystemExit

#__builtin__.object=customdec.TraceFunctions(object)
from tracing import E
e=E()
print E,type(type(E))











