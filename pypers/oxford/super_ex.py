# super_ex.py

from descriptor import AttributeDescriptor

class B(object):
   @staticmethod
   def sm(): return "staticmethod"

   @classmethod
   def cm(cls): return cls.__name__

   p = property()
   a = AttributeDescriptor()

class C(B): pass


