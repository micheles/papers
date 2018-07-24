def supermeth(meth): # not working with classmethods
    name = meth.__name__
    cls = meth.im_class
    inst = meth.im_self
    print "****", type(meth), cls, inst
    return getattr(super(cls, inst or cls), name)

from super_ex import B


class C1(B):
    pass

class C2(B):
    def __init__(self):
        print "C2.__init__"

class D(C1, C2):
    def __init__(self):
        print "D.__init__"



class E(D):
    def __init__(self):
        print "E.__init__"
        supermeth(E.__init__)(self)
    @classmethod
    def cm(cls):
        print super(E, cls).cm()
        return supermeth(E.cm)(cls)
    @staticmethod
    def sm():
        print super(E, E).sm()
        return supermeth(E.sm)()
    
e = E()
print
print E.sm()

