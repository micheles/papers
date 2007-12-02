# implementation with a strong usage of metaclasses

"""
A module to implement pep318 (decorator syntax) via magic doctrings.
For the documentation see

http://www.phyast.pitt.edu/~micheles/python/decorators,html

and the on-line help.
"""

import sys, re, inspect, __builtin__, noconflict
from types import FunctionType,ClassType
from noconflict import makecls
#from printerr import printerr

############################ UTILITIES ##############################
          
MAGICDOC=re.compile(r'\s*\[([\w_\s,]*)\]')
# regexp to recognize magic docstrings

class UnknownDecoratorError(Exception):
    "The name says it all"

class MetaDecorator(type):
    """Metaclass inducing a certain amount of magic on decorators:
    1. Each time a decorator is defined in any module, it is stored in
       MetaDecorator.dic and MetaDecorator.ls.
    2. If the (method) decorator has a 'get' method, a '__get__' method
       is automagically created as an alias to 'get'.
    3. Decorators calls are dispatched to the decorator _call_
       classmethod."""
    
    dic,ls={},[]
    
    def __init__(dec,*args):
        super(MetaDecorator,dec).__init__(*args)
        MetaDecorator.dic[dec.__name__]=dec
        MetaDecorator.ls.append(dec)
        get=dec.__dict__.get('get') # look at get
        if get: dec.__get__=get # alias __get__ to get

    def __call__(dec,*args):
        nargs=len(args)
        if nargs==1: # simple call of kind dec(obj)
            obj=args[0]
            if isinstance(obj,Decorator):
                dec=composed(dec,obj.__class__) # composed default decorator
            dec=decorator_from(obj.__doc__,dec)
        elif nargs==3: # class decorator called with three arguments
            dec=decorator_from(args[2].get('__doc__'),dec)
        return dec._call_(*args) # dispatch to the correct _call_ classmethod

def composed(*dclasses):
    dclasses=noconflict.remove_redundant(dclasses)
    decname=''.join([d.__name__ for d in dclasses])
    decorator=MetaDecorator.dic.get(decname)
    if not decorator: decorator=makecls()(decname,dclasses,{})
    return decorator
    
def decorator_from(docstring,defaultdec=None): 
    """Takes a magic docstring and a default decorator
    and returns a decorator class or None. It tries to be smart."""
    match=MAGICDOC.match(docstring or '')
    if not match: return defaultdec
    decnames=map(str.strip,match.group(1).split(','))
    try: dclasses=[MetaDecorator.dic[n] for n in decnames] # get the decorator
    except KeyError: raise UnknownDecoratorError(n) # classes from the names
    if defaultdec: dclasses.insert(0,defaultdec)
    return composed(*dclasses)
    
def decorate(objdict):
    """Takes an object with a dictionary and decorates all its functions
    and classes according to their docstrings."""
    for name,obj in objdict.__dict__.items():
        if getattr(obj,'__doc__',None): # non-empty docstring
            dec_obj=decorated(obj) or obj
            if dec_obj is not obj:
                setattr(dec_obj,'__klass__',objdict)
                setattr(objdict,name,dec_obj)

def get(docstring=None):
    "List of recognized decorators"
    dec=decorator_from(docstring,Decorator)
    isdecorator=lambda x: issubclass(x,dec)
    return filter(isdecorator,MetaDecorator.ls)

#################### functions of the API #################################

def decorated(obj):
    """If obj is a function or a class, returns a decorated object created
    according to obj's docstring or the original obj if no magic docstring
    is found; otherwise it returns None""" 
    if isinstance(obj,(FunctionType,ClassType,type)): # is function or class
        return Decorator(obj) or obj

def enhance_classes(docstring='[ClassDecorator]'):
    """Enhances all the classes in the caller module with a metaclass
    built from the given docstring."""
    callerglobals=sys._getframe(1).f_globals
    dec=decorator_from(docstring)
    callerglobals['__metaclass__']=dec
    callerglobals['object']=dec('object',(),{})
        
####################### BASIC DECORATORS ###########################

class Decorator(object):
    """Instance of MetaDecorator and mothers of all decorators. When called
    in the form Decorator(obj), with obj having a magic docstring, it returns
    an instance of the correct decorator, otherwise it returns None."""
    __metaclass__=MetaDecorator
    def _call_(dec,obj):
        "Returns None, all the interesting stuff is in MetaDecorator.__call__"
    _call_=classmethod(_call_)
    
class MethodDecorator(Decorator):
    """Descriptor class callable with a function or a descriptor object
    as argument. The calling syntax is redefined by the meta-metaclass
    MetaDecorator. It redefines__str__ and get (i.e. __get__) on its instances.
    """
    __klass__=type('?',(),{}) # dummy owner class, to be overridden
    def __init__(self,objfunc):
        "objfunc is a decorator object or a function"
        assert isinstance(objfunc,(FunctionType,Decorator)) 
        super(MethodDecorator,self).__init__(objfunc)
        self.__func__=getattr(objfunc,'__func__',objfunc)
    def get(self,obj,cls=None):
        "aliased to __get__, to be overridden"
        return self.__func__.__get__(obj,cls)
    def __str__(self):
        "Printing representation of kind <decoratorclass:functionname>"
        return '<%s:%s>' % (self.__class__.__name__,self.__func__.__name__)
    def _call_(dec,obj):
        "Returns a method decorator object."
        return type.__call__(dec,obj)  # calls __new__ and __init__
    _call_=classmethod(_call_)

    
class ClassDecorator(type,Decorator):
    """Metaclass callable with one or three arguments, having its calling
    syntax redefined by the meta-metaclass MetaDecorator. It redefines
    __str__ both on classes and instances."""
    def __init__(cls,name,bases,dic):
        super(ClassDecorator,cls).__init__(name,bases,dic)
        if cls.__str__ is object.__str__: # redefine default __str__
            cls.__str__=lambda self: "<%s instance>" % self.__class__.__name__
    def __str__(cls):
        return '<class %s[%s]>' % (cls.__name__,cls.__class__.__name__)
    def _call_(dec,*args):
        "Returns a decorated class"
        a=args[0] # first argument; can be a string or a class
        if inspect.isclass(a): args=a.__name__,a.__bases__,a.__dict__.copy() 
        return makecls(dec)(*args) 
    _call_=classmethod(_call_)

class Decorated(ClassDecorator):
    """Metaclass which decorates its instances"""
    def __init__(cls,name,bases,dic):
        super(Decorated,cls).__init__(name,bases,dic)
        decorate(cls)

#################### Built-in Method Decorators ######################

class staticmethod(MethodDecorator):
    "Decorator, converts a function in a staticmethod"
    def get(self,obj,cls=None):
        return super(staticmethod,self).get(obj,cls).im_func

class classmethod(MethodDecorator):
    "Decorator, converts a function in a classmethod"
    def get(self,obj,cls=None):
        if cls is None: cls=type(obj)
        return super(classmethod,self).get(cls,cls)
