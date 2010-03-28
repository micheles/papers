import re

class Regexp(object):
    def __init__(self, pattern):
        self._rx = re.compile(pattern)
        
class Setter(object):
    def __setattr__(self, name, value):
        val = "(?P<%s>%s)" % (name, value)
        super(Setter, self).__setattr__(name, val)

s = Setter()

s.paren_beg = r"\("
s.paren_end = r"\)"

print s.__dict__
