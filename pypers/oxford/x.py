class C(object):
    x = 1
    #def __getattr__(self, name):
    #    print "Trying to access a non-existing attribute"
    #    return name

    def __getattribute__(self, name):
        print "Trying to accessing %s" % name
        return name

    @staticmethod
    def f(): pass

    def m(self): pass
    
    



c = C()

print str(c)

print getattr(c, "x")
