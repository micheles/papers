"""
Does bound super have problems with special methods?
""" # seems not

"""
certainly problems with __self_class__, __thisclass__
"""
class B(object):
    def __init__(self):
        print "B.__init__"

class C(B):
    def __init__(self):
        print "C.__init__"
        self.__super.__init__()

C._C__super = super(C)

c = C()

print dir(c._C__super)
