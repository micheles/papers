# descr_example.py

class Descr(object):
    def __init__(self, attr):
        self.attr = attr
    def __get__(self, obj, objtyp = None):
        return self.attr

class M(type):
    a = 1 #Descr("I am an attribute descriptor")

class B:
    __metaclass__=M

class C(B):
    pass

print super(C,C()).a #=> I am an attribute descriptor



