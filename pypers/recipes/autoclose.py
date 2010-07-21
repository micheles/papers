import atexit

class AutoCloseMeta(type):
   _instances = [] # tracks the instances of this metaclass

   def __new__(mcl, name, bases, dic):
       cls = super(AutoCloseMeta, mcl).__new__(mcl, name, bases, dic)
       cls._closelst = [] # objects to be closed
       mcl._instances.append(cls)
       return cls

   def __call__(cls, *args, **kw):
       # tracks the instances of the instances
       if cls is AutoClose: # abstract base class
           raise TypeError('AutoClose is not meant to be instantiated!')
       self = super(AutoCloseMeta, cls).__call__(*args, **kw)
       cls._closelst.append(self)
       return self

   def closeall(cls):
       "Recursively close all instances of cls and its subclasses"
       # 'list' avoids a RuntimeError: dictionary changed size during iteration
       print 'Closing instances of %s' % cls # you may remove this print
       for obj in list(cls._closelst):
           obj.close()
       for subc in cls.__subclasses__():
           subc.closeall()

# assume the instantiation order is not important
class AutoClose(object):
   __metaclass__ = AutoCloseMeta
   def close(self):
       self._closelst.remove(self)

atexit.register(AutoClose.closeall)
