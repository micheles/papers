"""An example of protected methods."""

class B(object):
    def __init__(self):
        self.hello()
    def hello(self):
        print "hello!"

class C(B):
    def hello(self):
        print "cucu!"
        
b = B()
c = C()

##

class B(object):
    def __init__(self): # guaranteed to call the 'hello' method in THIS class
        self.__hello()
    def __hello(self):
        print "hello!"

class C(B):
    def __hello(self): # won't be called by B.__init__
        print "cucu!"
        
b = B()
c = C()

