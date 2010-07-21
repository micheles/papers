class MyClass(object):
    pass

myobj = MyClass()

for i in 1,2,3:
    def f(default=i):
        print default
    setattr(myobj, "a%s" % i, f)
    f()

print "-------"
myobj.a1()
myobj.a2()
myobj.a3()

