# library_using_super

class A(object):
    def __init__(self):
        print "A",
        super(A, self).__init__()

class B(object):
    def __init__(self):
        print "B",
        super(B, self).__init__()
