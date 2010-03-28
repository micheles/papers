# Simpler implementation with smaller usage of metaclasses

"""
A module to implement pep318 (decorator syntax) via magic doctrings.
It defines two new classes, MethodDecorator and ClassDecorator,
which are meant to be subclassed.

def f(x):
    "[chattymethod]"

as a shortcut for

def f(x):
    pass
f=chattymethod(f)

Decorators can be composed and for instance

def f(x):
    "[chattymethod,classmethod]"

is a shortcut for

def f(x):
    pass
f=chattymethodclassmethod(f)

where 'chattymethodclassmethod' is a decorator class obtained by
multiple inheriting from chattymethod and classmethod.
Notice that the built-in classmethod descriptor (idem for
staticmethod) is non=cooperatice, whereas the custom
descriptor chattymethod is cooperative: this means that
the custom descriptor has to be put *first* in the list.

The implementation stores the generated descriptors in a
dictionary, and avoids creating unneeded subclasses.

The names in the module are quite specific, since I am trying to avoid
troubles to people using the form "from decorator import *".
"""

import sys,re,inspect,__builtin__,time,noconflict
anyTrue=sum

############################ UTILITIES ##############################

class UnknownDecoratorError(Exception):
    "In case of mistakes"
    
class StoredDecorators(type):
    "Metaclass storing its instances in the dictionary dic"
    dic={}
    
    def __init__(cls,*args):
        super(StoredDecorators,cls).__init__(*args)
        StoredDecorators.dic[cls.__name__]=cls
        get=cls.__dict__.get('get') # if get
        if get: cls.__get__=get # set __get__

    def methodlike(mcl):
        "List of recognized MethodDecorators"
        return [name for name in mcl.dic
                if issubclass(mcl.dic[name],MethodDecorator)]
    methodlike=classmethod(methodlike)
    
    def classlike(mcl):
        "List of recognized ClassDecorators"
        return [name for name in mcl.dic
                if issubclass(mcl.dic[name],ClassDecorator)]
    classlike=classmethod(classlike)

def decorate(obj='THISMODULE'):
    """
    obj can be:
    - the string 'THISMODULE'
      in this case magic docstrings are interpreted in the new
      style classes of the calling module;
    - the string 'ALLMODULES'
      in this case magic docstrings are interpreted in the new
      style classes of ALL modules;
    - a dictionary or an object with a dictionary
      in this case magic docstrings are interpreted in all the
      functions and classes in the dictionary
    """
    dic={}
    if obj=='THISMODULE':
        callerglobals=sys._getframe(1).f_globals
        callerglobals['object']=_EnableMagicDocstrings
    elif obj=='ALLMODULES':
        __builtin__.object=_EnableMagicDocstrings
    elif isinstance(obj,dict):
        dic=obj
    elif hasattr(obj,'__dict__'):
        dic=obj.__dict__
    else:
        raise TypeError("Dictionary or object with a __dict__ required")
    for name,value in dic.items():
        _set(obj,name,value)

def _decorate(obj):
    """Given an object with a magic docstrings, returns its decorated
    version; otherwise, returns None"""
    docstring=inspect.getdoc(obj) or ''
    MO=re.match(r'\[([\w_ ,]*)\]',docstring)
    # for instance [name_1, name_2, name_3] is matched
    if MO:
        decorators=MO.group(1).split(',')
        try: dclasses=[StoredDecorators.dic[n] for n in decorators]
        except KeyError: raise UnknownDecoratorError(n)
        dclasses=noconflict.remove_redundant(dclasses)
        decname=''.join([d.__name__ for d in dclasses])
        decorator=StoredDecorators.dic.get(decname)
        if not decorator: decorator=makecls()(decname,dclasses,{})
        if issubclass(decorator,ClassDecorator): return decorator(obj)()
        return decorator(obj)

def _set(objdict,name,obj):
        dec=_decorate(obj)
        if dec:
            dec.inside=objdict
            if isinstance(objdict,dict):
                objdict[name]=dec
            else:
                setattr(objdict,name,dec)

class _MagicDocstrings:
    def __init__(cls,name,bases,dic):
        decorate(cls) # both cls and its dictionary
        
class _EnableMagicDocstrings:
    __metaclass__=_MagicDocstrings
    
class Decorator(object):
    """Instance of StoredDecorators, i.e. each time Decorator is
    subclassed, StoredDecorators.dic is updated. Provides a setattributes
    method to recognize magic attributes with ``set_`` prefix.
    Provides an 'inside' attribute (default='?')"""
    __metaclass__=StoredDecorators
    inside=type('?',(),{}) # default placeholder class (to be 
    # replaced by the class that contains the decorated method)
    def setattributes(self):
        for a in dir(self):
            if a.startswith('set_'):
                setattr(self,a[4:],getattr(self,a))
            
class MethodDecorator(Decorator):
    """MethodDecorator objects provide a 'get' method and a 'str' method"""
    def __init__(self,func):
        super(MethodDecorator,self).__init__(func)
        self.func=func; self.setattributes()
    def get(self,obj,cls=None): # default, to be overridden
        return self.func.__get__(obj,cls)
    def __str__(self):
        return '<%s:%s>' % (self.__class__.__name__,self.func.__name__)

class ClassDecorator(Decorator):
    """ClassDecorator takes a class as argument and returns a callable
    object acting as a factory of decorated objects"""
    def __init__(self,klass):
        super(ClassDecorator,self).__init__(klass)
        self.klass=klass; self.setattributes()
    def __call__(self): # to be cooperatively overridden
        return self.klass
    def __str__(self):
        return '<%s:%s>' % (self.__class__.__name__,self.klass.__name__)

#################### Useful Method Decorators ######################

class staticmethod(MethodDecorator):
    "Decorator, converts a function in a staticmethod"
    def get(self,obj,cls=None):
        super(staticmethod,self).get(obj,cls)
        return self.func

class classmethod(MethodDecorator):
    "Decorator, converts a function in a classmethod"
    def get(self,obj,cls=None):
        if cls is None: cls=type(obj)
        return super(classmethod,self).get(cls,cls)

class tracedmethod(MethodDecorator):
    "Descriptor class, converts a function in a traced method"
    indent=0; output=sys.stdout # defaults
    def __init__(self,func):
        super(tracedmethod,self).__init__(func)
        self.funcname=self.func.__name__
    def get(self,obj,cls): 
        clsname=self.inside.__name__
        boundmethod=super(tracedmethod,self).get(obj,cls) 
        def _(*args,**kw):
            i=' '*self.indent # default indentation
            self.__class__.indent+=4 # increases indentation
            self.output.write("%sCalling '%s.%s' with arguments " % 
                              (i,clsname,self.funcname))
            self.output.write("%s ...\n" % (str(args)+str(kw)))
            res=boundmethod(*args,**kw)
            self.output.write("%s'%s.%s' called with result: %s\n"
                              % (i,clsname,self.funcname,res))
            self.__class__.indent-=4 # restores default indentation
            return res
        return _

####################### Class Decorators ###############################

class Register(ClassDecorator):
    output=sys.stdout
    def __call__(self):
        cls=super(Register,self).__call__()
        print >> self.output, "%s: %s created" % (time.asctime(),cls)
        return cls
