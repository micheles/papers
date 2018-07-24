"An example of argument passing in cooperative methods"

class A(object):
    def __init__(self):
        print 'A'

class B(A):
    def __init__(self, a=None):
        print 'B with a=%s' % a
        super(B, self).__init__(a)

class C(A):
    def __init__(self, a):
        print 'C with a=%s' % a
        super(C, self).__init__()

class D(B, C):
    def __init__(self):
        print 'D'
        super(D, self).__init__()
