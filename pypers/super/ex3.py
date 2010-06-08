class B(object):
    def __repr__(self):
        return '<instance of %s>' % self.__class__.__name__
    def meth(self):
        print "B.meth(%s)" % self
    meth=classmethod(meth)

class C(B):
    def meth(self):
        print "C.meth(%s)" % self
        super(C,self).meth()
    meth=classmethod(meth)

class D(C):
    pass

d=D()

d.meth()
