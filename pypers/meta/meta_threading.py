from types import FunctionType
from threading import RLock

def makeThreadsafeMethod(func):
    def threadsafemethod(self, *args, **kwargs):
        self._monitor_lockObj.acquire()
        print ">>got lock"
        try:
            return func(self, *args, **kwargs)
        finally:
            self._monitor_lockObj.release()
            print "<<released lock"
    return threadsafemethod

def convert_methods(dic):
    methods=[ v for k,v in dic.iteritems() if isinstance(v, FunctionType) ]
    for m in methods:
        dic[m.__name__] = makeThreadsafeMethod(m)
    dic["_monitor_lockObj"] = RLock()


class ThreadSafeMethodsMetaClass(type):
     def __new__(meta, name, bases, dic):
         convert_methods(dic)
         return super(ThreadSafeMethodsMetaClass, meta).__new__(meta, name, bases, dic)

class MyClass(object):
     __metaclass__=ThreadSafeMethodsMetaClass

     def __init__(self):
         print "init!"
     def method(self, a1, a2):
         print a1,a2

m=MyClass()
m.method("irmen",42)
