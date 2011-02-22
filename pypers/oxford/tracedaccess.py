# tracedaccess.py

class TracedAccess(object):
    def __getattribute__(self, name):
        print "Accessing %s" % name
        return object.__getattribute__(self, name)


class C(TracedAccess):
    s = staticmethod(lambda : 'staticmethod')
    c = classmethod(lambda cls: 'classmethod')
    m = lambda self: 'method'
    a = "hello"


