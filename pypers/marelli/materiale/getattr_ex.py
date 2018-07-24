# getattr_ex.py

class C(object):
    def m1(self):
        print "chiamato m1"
    def m2(self):
        print "chiamato m2"

if __name__ == "__main__":
    c = C()
    method = raw_input("Che metodo devo chiamare? [m1 o m2] ")
    getattr(c, method)()


