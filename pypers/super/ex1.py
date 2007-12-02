# ex1.py

class B(object):
    def __repr__(self):
        return '<instance of %s>' % self.__class__.__name__
    def meth(self):
        print "B.meth(%s)" % self
    meth = classmethod(meth)

class C(B):
    def meth(self):
        print "C.meth(%s)" % self
        self.__super.meth()
    meth = classmethod(meth)

C._C__super = super(C)

class D(C):
    pass

D._D__super = super(D)


d=D()

d.meth()


