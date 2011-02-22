"""
Avoids metaclass conflicts by providing an additional built-in, makecls,
to be used as __metaclass__=makecls(*metaclasses). Returns a class factory.
"""

metadic={}

anyTrue=sum

def remove_redundant(bases): 
    ls=list(bases); nonredundant={}
    for c in bases:
        if anyTrue([issubclass(C,c) and c is not C for C in ls],
                   c in nonredundant):
            ls.remove(c) # if c is less specific or duplicated
        else: 
            nonredundant[c]=True
    return tuple(ls)

def _generatemetaclass(bases,metas,priority):
    metabs=tuple(map(type,bases))
    metabases=remove_redundant((metabs+metas, metas+metabs)[priority])
    if metabases in metadic: # already generated metaclass
        return metadic[metabases]
    elif not metabases: # trivial metabase
        meta=type 
    elif len(metabases)==1: # single metabase
        meta=metabases[0]
    else: # multiple metabases
        metaname=''.join([m.__name__ for m in metabases])
        meta=makecls()(metaname,metabases,{})
    return metadic.setdefault(metabases,meta)

def makecls(*metas,**options):
    """Class factory avoiding metatype conflicts. The invocation syntax is
    makecls(M1,M2,..,priority=1)(name,bases,dic). If the base classes have 
    metaclasses conflicting within themselves or with the given metaclasses, 
    it automatically generates a compatible metaclass and instantiate it. 
    If priority is True, the given metaclasses have priority over the 
    bases metaclasses."""

    priority=options.get('priority',False) # default, no priority
    def clsfactory(n,b,d):
        # workaround to allow redefinition of meta-meta.__call__
        meta=_generatemetaclass(b,metas,priority)
        return type.__call__(meta,n,b,d) # calls __new__ and __init__
    return clsfactory
          
#class Noconflict(type):
#    """Meta-metaclass tweaking the metaclass calling in such a way to
#    avoid conflicts"""
#    def __call__(mcl,n,b,d):
#        return makecls(mcl)(n,b,d)
      
if __name__=='__main__': # test              
    import __builtin__
    __builtin__.type=Noconflict('Type',(type,),{})
    class M1(type): pass
    class M2(type): pass
    class C1: __metaclass__=M1
    class C2: __metaclass__=M2
    class C(C1,C2): pass
    #__metaclass__=makecls()
#    assert type(C),__name__=='_M1M2'
