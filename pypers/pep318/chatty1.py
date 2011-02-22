# chatty1.py

import sys,customdec,decorators

class chattymethod1(customdec.chattymethod):
    def __init__(self,func):
        super(chattymethod1,self).__init__(func)
        self.logfile=self.logfile # class variable -> instance variable

class D:
    chattymethod1.logfile=sys.stdout
    def f(self): pass
    f=chattymethod1(f)

    chattymethod1.logfile=file('file.log','w')
    def g(self): pass
    g=chattymethod1(g) 

d=D()


