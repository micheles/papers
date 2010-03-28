

err=file('err','w')

def printerr(*args):
    "For debugging purposes"
    for a in args: print >> err, a,
    print >> err
                                                                    
def importmodule(name,dic):
    """Given a module name and a dictionary, executes the module in a copy
    of the dictionary and returns a new module."""
    already_imported=sys.modules.get(name)
    if already_imported: return already_imported # do nothing
    fname=name+'.py'
    dic=dic.copy()
    execfile(fname,dic)
    mod=types.ModuleType(name)
    for k,v in dic.iteritems():
        setattr(mod,k,v)
    sys.modules[name]=mod
    mod.__name__=name

    mod.__file__=fname
    return mod

class ModuleDecorator(Decorator,types.ModuleType):
    def __init__(self,mod): # non-cooperative
        self.undecorated=mod
        for k,v in mod.__dict__.iteritems():
            setattr(self,k,v)
        decorate(self)
    def __str__(self):
        return '<module %s[%s]>' % (self.mod.__name__,self.__class__.__name__)

class DecoratedModule(ModuleDecorator): # defined for readability
    pass

def callModuleDecorator(dec,*args):
    if issubclass(dec,ModuleDecorator):
        nargs=len(args)
        if nargs==1:
            mod=args[0]
        elif nargs==2:
            name,glob=args # a different object for each module
            glob['object']=ClassDecorator(object)
            mod=importmodule(name,glob)
        else:
            raise TypeError('%s() takes 1 or 2 arguments' % dec.__name__)
        return type.__call__(dec,mod)
    
