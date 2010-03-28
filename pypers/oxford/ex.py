class Base(object):
    def __init__(self):
        print "B.__init__"

class MyClass(Base):
    "I do not cooperate with others"
    def __init__(self):
        print "MyClass.__init__"
        Base.__init__(self)  #instead of super(MyClass, self).__init__()


class Mixin(Base):
    "I am cooperative with others"
    def __init__(self):
        print "Mixin.__init__"
        super(Mixin, self).__init__()


class HerClass(MyClass, Mixin):
    "I am cooperative too"
    def __init__(self):
        print "HerClass.__init__"
        super(HerClass, self).__init__()


h = HerClass()
