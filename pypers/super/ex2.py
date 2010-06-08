# ex2.py# fixed in 2.4

 class C(object):
     pass

 C.s = super(C) 

 if __name__ == "__main__":
     import doctest, __main__
     doctest.testmod(__main__)

 
