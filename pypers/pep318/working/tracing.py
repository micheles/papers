# tracing.py

import customdec; customdec.enhance_classes("[Decorated]")
 
class B(object): 
    def __init__(self):
        "[tracedmethod]"
        super(B,self).__init__()

class D(object): 
    def __init__(self):
        "[tracedmethod]"
        super(D,self).__init__()

class E(B,D):
    def __init__(self):
        "[tracedmethod]"
        super(E,self).__init__()

 
