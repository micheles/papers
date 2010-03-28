# example5.py

import decorators; decorators.enhance_classes()

class C:
    "[Decorated]" # required docstring 
    def identity(x):
        "[staticmethod]"
        return x
    class Inner:
        "[Decorated]" # required docstring   
        def name(cls):
            "[classmethod]"
            return cls.__name__


assert C.identity(1) == 1
assert C.Inner.name() == 'Inner'


