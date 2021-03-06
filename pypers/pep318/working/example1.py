# example1.py

import decorators

def do_nothing(self):
   "No magic docstring here"
dec_do_nothing=decorators.decorated(do_nothing)

def identity(x):
    "[staticmethod]"
    return x
dec_identity=decorators.decorated(identity) 

def name(cls):
    "[classmethod]"
    return cls.__name__
dec_name=decorators.decorated(name)

class OldStyle:
    do_nothing=dec_do_nothing
    identity=dec_identity

class NewStyle(object):
    name=dec_name

o=OldStyle() # creates an old style instance
n=NewStyle() # creates a new style instance


