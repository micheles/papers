# example6.py

"How to trace a class method"

import customdec; customdec.enhance_classes()

class C(object):
    "[Decorated]"
    def fact(cls,n): # a traced classmethod
        "[classmethod,tracedmethod]"
        if n==0: return 1
        else: return n*cls.fact(n-1)


