"[Decorated]"

import decorators,customdec; decorators.decorated()

class desc(object):
    def __get__(self,obj,cls):
        print obj,cls
        
class C(object):
    def f(cls):
        "[classmethod]"
        print cls
    g=desc()


C.g
C().g
