# _main.py

import sys

def classinitializer(proc):
  # basic idea stolen from zope.interface.advice, which credits P.J. Eby
  def newproc(*args, **kw):
      frame = sys._getframe(1)
      if '__module__' in frame.f_locals and not \
         '__module__' in frame.f_code.co_varnames: # we are in a class
          if '__metaclass__' in frame.f_locals: 
            raise SyntaxError("Don't use two class initializers or\n"
            "a class initializer together with a__metaclass__ hook")
          def makecls(name, bases, dic):
             try:
                cls = type(name, bases, dic)
             except TypeError, e:
                if "can't have only classic bases" in str(e):
                   cls = type(name, bases + (object,), dic)
                else: # other strange errors, such as __slots__ conflicts, etc
                   raise
             proc(cls, *args, **kw)
             return cls
          frame.f_locals["__metaclass__"] = makecls
      else:
          proc(*args, **kw)
  newproc.__name__ = proc.__name__
  newproc.__module__ = proc.__module__
  newproc.__doc__ = proc.__doc__
  newproc.__dict__ = proc.__dict__
  return newproc
  


import datetime

@classinitializer
def def_properties(cls, schema):
    """
    Add properties to cls, according to the schema, which is a list
    of pairs (fieldname, typecast). A typecast is a
    callable converting the field value into a Python type.
    The initializer saves the attribute names in a list cls.fields
    and the typecasts in a list cls.types. Instances of cls are expected 
    to have private attributes with names determined by the field names.
    """
    cls.fields = []
    cls.types = []
    for name, typecast in schema:
        if hasattr(cls, name): # avoid accidental overriding
            raise AttributeError('You are overriding %s!' % name)
        def getter(self, name=name):
            return getattr(self, '_' + name)
        def setter(self, value, name=name, typecast=typecast):
            setattr(self, '_' + name, typecast(value)) 
        setattr(cls, name, property(getter, setter))
        cls.fields.append(name)
        cls.types.append(typecast)

def date(isodate): # add error checking if you like
    "Convert an ISO date into a datetime.date object"
    return datetime.date(*map(int, isodate.split('-')))


