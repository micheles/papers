# tracing.py

"""
This is a pre-existing module not using decorators but using multiple
inheritance and unsafe metaclasses. We want to trace it for debugging 
purposes.
"""
 
class M(type): 
    "There should be some non-trivial code here"

class B(object): 
    def __init__(self):
        super(B,self).__init__()

class D(object): 
    __metaclass__=M
    def __init__(self):
        super(D,self).__init__()

class E(B,D):
    def __init__(self):
        super(E,self).__init__()

 
