from oopp import isplaindata,inspect
class TracedAccess(type):
    "Metaclass converting data attributes to properties"
    def __init__(cls,name,bases,dic):
        cls.datadic={}
        for a in dic:
            if isplaindata(a):
                cls.datadic[a]=dic[a]
                def get(self,a=a):
                    v=cls.datadic[a]
                    print "Accessing %s, value=%s" % (a,v)
                    return v
                def set(self,v,a=a):
                    print "Setting %s, value=%s" % (a,v)
                    cls.datadic[a]=v
                setattr(cls,a,property(get,set))
class C(object):
    __metaclass__ = TracedAccess
    a1='x'
class D(C): # shows that the approach works well with inheritance
    a2='y'
i=D()
i.a1 # => Accessing a1, value=x
i.a2 # => Accessing a2, value=y
i.a1='z' # => Setting a1, value=z
i.a1 # => Accessing a1, value=z
