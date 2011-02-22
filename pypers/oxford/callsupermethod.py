class B(object):
    def __new__(cls, *args, **kw):
        print "B.__new__"
        return super(B, cls).__new__(cls, *args, **kw)
    def __init__(self, *args, **kw):
        print "B.__init__"
        super(B, self).__init__(*args, **kw)

########################################################################






















































from magicsuper import object

class B(object):
    def __new__(cls, *args, **kw):
        print "B.__new__"
        return callsupermethod(cls, *args, **kw)
    def __init__(self, *args, **kw):
        print "B.__init__"
        callsupermethod(*args, **kw)




























    @staticmethod
    def sm():
        print "B.sm"
    @classmethod
    def cm(cls):
        print cls.__name__
    
    
class C(B):
    def __new__(cls, *args, **kw):
        print args, kw
        return callsupermethod(cls, *args, **kw)
    @staticmethod
    def sm():
        callsupermethod()
    @classmethod
    def cm(cls):
        callsupermethod()
    

c = C(1, x=2)
c.sm()
c.cm()
