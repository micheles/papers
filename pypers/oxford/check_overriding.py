# check_overriding.py

class Base(object):
    a = 0

class CheckOverriding(type):
    "Prints a message if we are overriding a name."
    def __new__(mcl, name, bases, dic):
        for name, val in dic.iteritems():
            if name.startswith("__") and name.endswith("__"): 
                continue # ignore special names
            a_base_has_name = True in (hasattr(base, name) for base in bases)
            if a_base_has_name:
                print "AlreadyDefinedNameWarning: " + name
        return super(CheckOverriding, mcl).__new__(mcl, name, bases, dic)

class MyClass(Base):
    __metaclass__ = CheckOverriding
    a = 1

class ChildClass(MyClass):
    a = 2


