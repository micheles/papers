# super_old_new.py
class O:
    def __init__(self):
        print "O.__init__"

class N(O, object):
    def __init__(self):
        print "N.__init__"
        super(N, self).__init__()


