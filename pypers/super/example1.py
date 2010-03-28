# example1.py

class M(type):
    "A metaclass with a class attribute 'a'."
    a = 1 

class B:
    "An instance of M with a meta-attribute 'a'."
    __metaclass__ = M

class C(B):
    "An instance of M with the same meta-attribute 'a'"

if __name__ == "__main__":
    print B.a, C.a # => 1 1 
    print super(C,C).a #=> attribute error


