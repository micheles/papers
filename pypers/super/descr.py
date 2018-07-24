# descr.py

class Descr(object):
    def __init__(self, a):
        self.a = a
    def __get__(self, obj, objtyp):
        return self.a

class C(object):
    p=property()

#C.s = super(C, C()) 

if __name__ == "__main__":
    import doctest, __main__
    doctest.testmod(__main__)
