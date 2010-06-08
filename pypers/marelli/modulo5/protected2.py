class B(object):
    def __p(self):
        print "hello!"
    def __init__(self):
        self.__p()

class C(B):
    def __init__(self):
        self.__p()

c = C()
