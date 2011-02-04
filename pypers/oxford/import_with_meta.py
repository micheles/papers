# import_with_metaclass.py

import inspect, types

def import_with_metaclass(metaclass, modname):
    "Module importer substituting custom metaclass"
    dct = {'__module__' : modname}
    mod = __import__(modname)
    for key, val in mod.__dict__.items():
        if inspect.isclass(val):
            if isinstance(val, types.ClassType):
                bases = (val, object) # convert old-style to new-style
            else:
                bases = (val,)
            setattr(mod, key, metaclass(key, bases, dct))
    return mod


