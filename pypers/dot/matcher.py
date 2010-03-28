"""A regular expression based matcher"""

# needs work to get something smart (maybe)

import re

class MetaMatcher(type):
    def __init__(cls,name,bases,dic):
        cls.rxs="(%s) % "")|(".join([func.rx for name,func in dic.iteritems()])
        cls.rxo=re.compile(cls.rxs)
    
class Matcher(object):
    __metaclass__=MetaMatcher
    def __init__(self, text):
        self.it=self.__class__.rxo.finditer(text)
    def __iter__(self):
        return sel
    def next(self):
        return self.it.next()
