# walk.py

def walk(iterable, level=0):
    for obj in iterable:
        if not hasattr(obj, "__iter__"): # atomic object
            yield obj, level
        else: # composed object: iterate again
            for subobj, lvl in walk(obj, level + 1):
                yield subobj, lvl

def flatten(iterable):
    return (obj for obj, level in walk(iterable))
       
def pprint(iterable):
    for obj, level in walk(iterable):
        print " "*level, obj
       

