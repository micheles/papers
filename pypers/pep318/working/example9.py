# example9.py

import customdec; customdec.enhance_classes()

logfile=file('file3.log','w')

class C(object):
    "[Decorated]"
    def fact(self,n):
        "[tracedmethod] The good old factorial."
        if n==0: return 1
        else: return n*self.fact(n-1)
    fact.output=logfile

C().fact(2) # write a message to logfile

logfile.close()


