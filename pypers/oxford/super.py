class B(object):
    def __init__(self):
        print "B.__init__"

class C(B):
    def __init__(self):
        #super(C, self).__init__()
        B.__init__(self)
        print "C.__init__"

######################################
        
class herB(object):
    pass

def __init__(self):
    super(self.__class__, self).__init__()
    print "herB.__init__"

herB.__init__ = __init__

class herC(herB, C):
    pass
        

c = herC()
