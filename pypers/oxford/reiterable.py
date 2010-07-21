# reiterable.py

class ReIter(object):
    "A re-iterable object."
    def __iter__(self):
        yield 1
        yield 2
        yield 3


