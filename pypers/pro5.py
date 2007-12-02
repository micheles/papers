class M(type):
    def __init__(cls,name,bases,dic):
        print 'called!'

class C(object):
    pass

C.__class__=M

class D(C): pass
