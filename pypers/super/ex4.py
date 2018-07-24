class B(object):
    pass

class C(B):
    s=super(B)

from pydoc import help

help(C)
