class M(type):
    pass

class B(object):
    __metaclass__=M
    a='class attribute'


class C(B):
    pass

print super(C,C).a


print super(M,M).__name__

print super(M,C).a

