# trace_builtin.py

class TracedAccess1(object):
    def __getattribute__(self, name):
        print "1: accessing %s" % name
        return super(TracedAccess1, self).__getattribute__(name)

class TracedAccess2(object):
    def __getattribute__(self, name):
        print "2: accessing %s" % name
        return super(TracedAccess2, self).__getattribute__(name)

class B(object):
    def __init__(self, *args):
        super(B, self).__init__(*args)



