# dictmixin.py

from metatracer import MetaTracer
from UserDict import DictMixin

class TracedDM(DictMixin, object):
    __metaclass__ = MetaTracer
    def __getitem__(self, item):
        return item
    def keys(self): 
        return [1,2,3]


