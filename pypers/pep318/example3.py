# example3.py

import decorators; decorators.enhance_classes()

class C:
    "[Decorated]" # magic docstring here 
    def do_nothing(self):
       "No magic docstring here"

    def identity(x):
        "[staticmethod]"
        return x

class D(object):
    "Undecorated" # no magic docstring here
    def name(cls):
        "[classmethod]"
        return cls.__name__

c=C(); d=D()


