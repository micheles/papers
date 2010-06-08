# noconflict.py

def memoize(f): #NOT WORKING
    """This closure remembers all f invocations"""
    argskw,result = [],[]
    def _(*args,**kw): 
        akw=args,kw
        try: # returns a previously stored result
            return result[argskw.index(akw)]
        except ValueError: # there is no previously stored result
            argskw.append(akw) # update argskw
            result.append(f(*args,**kw)) # update result
            return result[-1] # return the new result
    _.argskw=argskw #makes the argskw list accessible outside
    _.result=result #makes the result list accessible outside
    return _

def memoize(f): # WORKING
    """This closure stores all f invocations in a dictionary; notice that
    f cannot have keywords arguments or dictionaries as arguments."""
    dic={} # internal dictionary
    wrapped_f=lambda *args: dic.setdefault(args,f(*args))
    wrapped_f.dic=dic # make dic available outside
    return wrapped_f

def _generatemetaclass(bases,metas,priority):
    trivial=lambda m: sum([issubclass(M,m) for M in metas],m is type)
    # hackish!! m is trivial if it is 'type' or, in the case explicit
    # metaclasses are given, if it is a superclass of at least one of them
    metabases=tuple([mb for mb in map(type,bases) if not trivial(mb)])
    metabases=(metabases+metas, metas+metabases)[priority]
    metaname="_"+''.join([m.__name__ for m in metabases])
    if not metabases: # trivial metabase
        return type 
    elif len(metabases)==1: # single metabase
        return metabases[0]
    else: # multiple metabases
        return clsfactory()(metaname,metabases,{})

_generatemetaclass=memoize(_generatemetaclass) 

def clsfactory(*metas,**options):
    """Class factory avoiding metatype conflicts. The invocation syntax is
    clsfactory(M1,M2,..,priority=1)(name,bases,dic). If the base classes have 
    metaclasses conflicting within themselves or with the given metaclasses, 
    it automatically generates a compatible metaclass and instantiate it. 
    If priority is True, the given metaclasses have priority over the 
    bases' metaclasses"""

    priority=options.get('priority',False) # default, no priority
    return lambda n,b,d: _generatemetaclass(b,metas,priority)(n,b,d)

# solving the meta-metaclass conflict
class MM1(type): pass
class MM2(type): pass
class M1(type):
    __metaclass__=MM1
class M2(type): 
    __metaclass__=MM2
class A: __metaclass__=M1
class B: __metaclass__=M2
class C(A,B):
    __metaclass__=clsfactory()
print C,type(C),type(type(C))
#=> <class '__main__.C'> <class '__main__._M1M2'> <class '__main__._MM1MM2'>

from oopp import pretty
print pretty(_generatemetaclass.dic)

#print _generatemetaclass.result
#print _generatemetaclass.argskw

class D(A,B):
    __metaclass__=clsfactory()


print type(D) is type(C)

# skipping unneeded metaclasses
class M(type): pass
class M1(M): pass

class B: __metaclass__=M
class C(B): 
    __metaclass__=clsfactory(M1)

print C,type(C),type(type(C))
#=> <class '__main__.C'> <class '__main__.M1'> <type 'type'>
