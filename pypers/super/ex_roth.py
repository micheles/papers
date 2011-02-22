class C(object):
    @staticmethod
    def f():
        print "C.f"

class D(C):
    @staticmethod
    def f():
        print "D.f"
        super(D, D).f()

D.f()
