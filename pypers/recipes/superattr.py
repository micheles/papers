class Super(object):
    def __init__(self, cls, meth):
        self.cls = cls
        self.meth = meth
    def __call__(self, *args, **kw):
        return getattr(super(self.cls, obj or objtype), self.meth)
    
class B(object):
    def __init__(self):
        print "B.__init__"

class C(object):
    def __init__(self):
        print "C.__init__"
        print self.__init__.super(self)

C.__dict__["__init__"].super = Super(C, "__init__")


c = C()
