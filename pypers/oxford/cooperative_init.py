# cooperative_init.py

"""Given a hierarchy, makes __init__ cooperative.
The only change needed is to add a line

   __metaclass__ = CooperativeInit

to the base class of your hierarchy."""

from decorators import decorator 

class CooperativeInit(type):
    def __init__(cls, name, bases, dic):

        @decorator
        def make_cooperative(__init__, self, *args, **kw):
            super(cls, self).__init__(*args, **kw)
            __init__(self, *args, **kw)

        __init__ = dic.get("__init__")
        if __init__:
            cls.__init__ = make_cooperative(__init__)

class Base:
    __metaclass__ = CooperativeInit
    def __init__(self):
        print "B.__init__"

class C1(Base):
    def __init__(self):
        print "C1.__init__"

class C2(Base):
   def __init__(self):
       print "C2.__init__"

class D(C1, C2):
    def __init__(self):
        print "D.__init__"


