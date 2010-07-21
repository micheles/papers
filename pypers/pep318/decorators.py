# decorators.py

"""
A module to implement pep318 (decorator syntax) via magic doctrings.
For the full documentation see
http://www.phyast.pitt.edu/~micheles/python/decorators.html .
"""

import sys, re, inspect
from types import FunctionType,ClassType
from safetype import remove_redundant,__type__,safetype as type

#from printerr import printerr

############################ UTILITIES ##############################
          
MAGICDOC=re.compile(r'\s*\[([\w_\s,]*)\]')
#regexp to recognize magic docstrings

class UnknownDecoratorError(Exception):
    "The name says it all."

class MetaDecorator(type):
    """Metaclass inducing a certain amount of magic on decorators:
    
    1. each time a decorator class is defined in any module, it is stored in
       MetaDecorator.dic and MetaDecorator.ls;
    2. if the decorator class has a 'get' method, a '__get__' method
       is automagically created as an alias to 'get';
    3. decorators calls are dispatched to the decorator _call_
       classmethod.
    The net effect is that the call decoratorclass(...) returns an
    instance of a *subclass* of decoratorclass.
    """
    
    dic,ls={},[]
    
    def __init__(dec,*args):
        super(MetaDecorator,dec).__init__(*args)
        MetaDecorator.dic[dec.__name__]=dec
        MetaDecorator.ls.append(dec)
        get=dec.__dict__.get('get') # look at get
        if get: dec.__get__=get # alias __get__ to get

    def __call__(dec,*args):
        """This is the heart of the module. Infers the correct decorator class
        from ``dec`` and the docstring and creates the correct decorator
        object. Returns the original object if ``dec`` is the trivial
        ``Decorator`` class and no docstring is found."""
        nargs=len(args)
        if nargs==1: # simple call of kind dec(obj)
            obj=args[0]
            if not isinstance( # not method decorator, function or class
                obj,(MethodDecorator,FunctionType,ClassType,__type__)):
                return # do nothing
            elif isinstance(obj,decorator): # compose with obj.__class__ too
                dec=compose(decorators_in(obj.__doc__), dec, obj.__class__)
            else: # compose with dec only
                dec=compose(decorators_in(obj.__doc__), dec)
        elif nargs==3: # class decorator called with three arguments
            dec=compose(decorators_in(args[2].get('__doc__')),dec)
        return dec._call_(*args) # dispatch to the correct _call_ classmethod

def compose(dclasses,*defaultclasses):
    """Retrieves or creates a decorator for a tuple of decorators. If
    defaults decorators are given, they get the precedence. It removes
    redundant classes."""
    dclasses=remove_redundant(defaultclasses+dclasses)
    decname=''.join([d.__name__ for d in dclasses])
    dec=MetaDecorator.dic.get(decname)
    if not dec: dec=type(decname,dclasses,{})
    return dec
    
def decorators_in(docstring): 
    """Takes a docstring and returns a (possibly empty) tuple of decorator
    classes."""
    match=MAGICDOC.match(docstring or '')
    if not match: return ()
    decnames=map(str.strip,match.group(1).split(',')) # get the names
    try: dclasses=[MetaDecorator.dic[n] for n in decnames] # get the decorators
    except KeyError: raise UnknownDecoratorError(n) 
    return tuple(dclasses)
    
def decorate(objdict):
    """Takes an object with a dictionary and decorates all its functions
    and classes according to their docstrings."""
    for name,obj in objdict.__dict__.items(): # works for classes too
        #if getattr(obj,'__doc__',None): # non-empty docstring
        dec_obj=decorator(obj)
        if dec_obj:
            setattr(dec_obj,'__klass__',objdict)
            setattr(objdict,name,dec_obj)

def getdec(magicstring="[decorator]"):
    """Given a magicstring describing a valid decorator name, returns the list
    of its subclasses. By default returns the full list of decorators."""
    dec=compose(decorators_in(magicstring))
    isdecorator=lambda x: issubclass(x,dec)
    return filter(isdecorator,MetaDecorator.ls)

def instance_str(self):
    "Redefines the printing representation of simple objects"
    return "<%s instance>" % self.__class__.__name__
    
####################### BASIC DECORATORS ###########################

class decorator(object):
    """Instance of MetaDecorator and mothers of all decorators. When called
    in the form ``decorator(obj)`` with obj having a magic docstring, it
    returns a decorated object; otherwise it returns ``None``."""
    __metaclass__=MetaDecorator
    def _call_(dec,obj):
        "Returns None, all the interesting stuff is in MetaDecorator.__call__"
    _call_=classmethod(_call_)

class MethodDecorator(decorator):
    """Descriptor class callable with a function or a descriptor object
    as argument. It defines a default printing representation on method
    decorators objects and a default ``get``  method. All the rest is
    provided by the metaclass ``MetaDecorator``.
    """
    __klass__=type('?',(),{}) # dummy definition class, to be overridden
    def __init__(self,objfunc):
        "objfunc is a decorator object or a function"
        assert isinstance(objfunc,(FunctionType,decorator)) 
        super(MethodDecorator,self).__init__(objfunc)
        self.__func__=getattr(objfunc,'__func__',objfunc)
    def get(self,obj,cls=None):
        "aliased to __get__ by the metaclass, to be overridden"
        return self.__func__.__get__(obj,cls)
    def __str__(self):
        "Printing representation of kind <decoratorclass:functionname>"
        return '<%s:%s>' % (self.__class__.__name__,self.__func__.__name__)
    def _call_(dec,obj):
        "Returns a method decorator object."
        return type.__call__(dec,obj)  # calls __new__ and __init__
    _call_=classmethod(_call_)

class ClassDecorator(type,decorator):
    """Metaclass callable with one or three arguments, having its calling
    syntax redefined by ``MetaDecorator``."""
    def _call_(dec,*args):
        "Returns a decorated class"
        a=args[0] # first argument; must be a string or a class
        if inspect.isclass(a): args=a.__name__,a.__bases__,a.__dict__.copy() 
        return type.__call__(dec,*args) # calls __new__ and __init__
    _call_=classmethod(_call_)

class Decorated(ClassDecorator):
    """Metaclass which decorates the methods of its instances according
    to the docstrings. It redefines ``__str__`` on
    its instances and the default ``__str__`` on the instances of its
    instances."""
    def __init__(cls,name,bases,dic):
        super(Decorated,cls).__init__(name,bases,dic)
        if cls.__str__ is object.__str__: cls.__str__=instance_str
        # redefine default __str__
        decorate(cls)
    def __str__(cls):
        "Redefine the printing representation of classes"
        return '<class %s[%s]>' % (cls.__name__,cls.__class__.__name__)

#################### Built-in Method decorators ######################

class staticmethod(MethodDecorator):
    "Decorator, converts a function in a staticmethod"
    def get(self,obj,cls=None):
        return super(staticmethod,self).get(obj,cls).im_func

class classmethod(MethodDecorator):
    "Decorator, converts a function in a classmethod"
    def get(self,obj,cls=None):
        if cls is None: cls=type(obj)
        return super(classmethod,self).get(cls,cls)

#################### functions of the API #################################

def enhance_classes(magicstring='[ClassDecorator]'):
    """Enhances all the classes in the caller module with a metaclass
    built from the given magicstring."""
    callerglobals=sys._getframe(1).f_globals
    dec=compose(decorators_in(magicstring))
    callerglobals['__metaclass__']=dec
    callerglobals['object']=dec('object',(),{})
