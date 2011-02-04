# magicprop.py

class MagicProperties(type):
    def __init__(cls, name, bases, dic):
        prop_names = set(name[3:] for name in dic
                         if name.startswith("get")
                         or name.startswith("set"))
        for name in prop_names:
            getter = getattr(cls, "get" + name, None)
            setter = getattr(cls, "set" + name, None)
            setattr(cls, name, property(getter, setter))

class Base(object):
    __metaclass__ = MagicProperties
    def getx(self):
        return self._x
    def setx(self, value):
        self._x = value

class Child(Base):
    def getx(self):
        print "getting x"
        return super(Child, self).getx() 
    def setx(self, value):
        print "setting x"
        super(Child, self).setx(value) 


