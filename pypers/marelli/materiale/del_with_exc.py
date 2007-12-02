# del_with_exc.py

class C(object):
    def __del__(self):
        print "Hai chiamato del"

c = C()
raise RuntimeError("Ahi ahi!")


