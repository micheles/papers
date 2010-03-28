# descriptor.py


class AttributeDescriptor(object):
   def __get__(self, obj, cls=None):
       if obj is None and cls is None:
           raise TypeError("__get__(None, None) is invalid")
       elif obj is None:
           return self.get_from_class(cls)
       else:
           return self.get_from_obj(obj)
   def get_from_class(self, cls):
       print "Getting %s from %s" % (self, cls)
   def get_from_obj(self, obj):
       print "Getting %s from %s" % (self, obj)


class Staticmethod(AttributeDescriptor):
   def __init__(self, func):
       self.func = func
   def get_from_class(self, cls):
       return self.func
   get_from_obj = get_from_class


class Classmethod(AttributeDescriptor):
   def __init__(self, func):
       self.func = func
   def get_from_class(self, cls):
       return self.func.__get__(cls, type(cls))
   def get_from_obj(self, obj):
       return self.get_from_class(obj.__class__)

class C(object):
   s = Staticmethod(lambda : 1)
   c = Classmethod(lambda cls : cls.__name__)

c = C()

assert C.s() == c.s() == 1
assert C.c() == c.c() == "C"


