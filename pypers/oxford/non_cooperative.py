# non_cooperative.py

class B1(object):
    def __init__(self, **kw):
        print "B1.__init__"
        super(B1, self).__init__(**kw)

class B2(object):
    def __init__(self, **kw):
        print "B2.__init__"
        super(B2, self).__init__(**kw)


