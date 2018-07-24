
## def flatten(container):
##     for obj in container:
##         if hasattr(obj, "__iter__"):
##             for el in flatten(obj):
##                 yield el
##         else:
##             yield obj

def walk(container, level=0):
    for obj in container:
        if not hasattr(obj, "__iter__"):
            yield obj, level
        else:
            for subobj, lvl in walk(obj, level + 1):
                yield subobj, lvl

def pprint(container):
    for obj, level in walk(container):
        print " "*level, obj

def flatten(container):
    return (obj for obj, level in walk(container))
        
nested_ls = [1,[2,[3,[[[4,5],6]]]],7]
pprint(nested_ls)
pprint(flatten(nested_ls))
