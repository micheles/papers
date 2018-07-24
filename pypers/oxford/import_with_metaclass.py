# import_with_metaclass.py
"""
``import_with_metaclass(metaclass, modulepath)`` generates
a new module from and old module, by enhancing all of its classes.
This is not perfect, but it should give you a start."""

import os, sys, inspect, types

def import_with_metaclass(metaclass, modulepath):
    modname = os.path.basename(modulepath)[:-3] # simplistic
    mod = types.ModuleType(modname)
    locs = dict(
        __module__ = modname,
        __metaclass__ = metaclass,
        object = metaclass("object", (), {}))
    execfile(modulepath, locs)
    for k, v in locs.iteritems():
        if inspect.isclass(v): # otherwise it would be "__builtin__"
            v.__module__ = "__dynamic__"
        setattr(mod, k, v)
    return mod
 

