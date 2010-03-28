from magicsuper import object

class B(object):
    def __init__(self):
        print "B.__init__"
        super(B, self).__init__()


class C(B):
    def __init__(self):
        print "C.__init__"
        callsupermethod()


c = C()


        
