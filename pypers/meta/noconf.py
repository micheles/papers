# noconflict.py

metadic={}

def _generatemetaclass(bases,metas,priority):
    trivial=lambda m: sum([issubclass(M,m) for M in metas],m is type)
    # hackish!! m is trivial if it is 'type' or, in the case explicit
    # metaclasses are given, if it is a superclass of at least one of them
    metabs=tuple([mb for mb in map(type,bases) if not trivial(mb)])
    metabases=(metabs+metas, metas+metabs)[priority]
    if metabases in metadic: # already generated metaclass
        return metadic[metabases]
    elif not metabases: # trivial metabase
        meta=type 
    elif len(metabases)==1: # single metabase
        meta=metabases[0]
    else: # multiple metabases
        metaname="_"+''.join([m.__name__ for m in metabases])
        meta=makecls()(metaname,metabases,{})
    return metadic.setdefault(metabases,meta)

def makecls(*metas,**options):
    """Class factory avoiding metatype conflicts. The invocation syntax is
    makecls(M1,M2,..,priority=1)(name,bases,dic). If the base classes have 
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
    __metaclass__=makecls()
#print C,type(C),type(type(C))
#=> <class '__main__.C'> <class '__main__._M1M2'> <class '__main__._MM1MM2'>

#from oopp import pretty
#print pretty(_generatemetaclass.dic)

#print _generatemetaclass.result
#print _generatemetaclass.argskw

class D(A,B):
    __metaclass__=makecls()

#print type(D) is type(C)

# skipping unneeded metaclasses
class M(type): pass
class M1(M): pass

class B: __metaclass__=M
class C(B): 
    __metaclass__=makecls(M1)

#print C,type(C),type(type(C))
#=> <class '__main__.C'> <class '__main__.M1'> <type 'type'>

# reuse already generated metaclasses
class M1(type): pass
class M2(type): pass
class B1(object): __metaclass__=M1
class B2(object): __metaclass__=M2
class B3(B2): pass
class C1(B1,B2): __metaclass__=makecls()
class C2(B1,B3): __metaclass__=makecls()

print type(C1) is type(C2)
