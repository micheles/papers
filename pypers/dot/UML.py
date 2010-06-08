"""Draws UML diagrams using dot"""

# dot.py

import inspect #contents=inspect.classify_class_attrs(cls)

if "skeleton":
    viewable = lambda name : False
else:
    viewable = lambda name : \
               not (name.startswith("_") or name.endswith("__roles__"))
    
def get_attrs(cls):
    return "\\n".join(filter(viewable,cls.__dict__.iterkeys()))

#def get_attrs(cls): return ""

def dotcode(cls):
    setup='node [color=Green,fontcolor=Blue,fontname=Courier]\n'
    name='hierarchy_of_%s' % cls.__name__
    code='\n'.join(codegenerator(cls))
    page='page="8.3,11.7"' # split pages
    #page="" # to have all in one page
    return "digraph %s{\n%s;\n%s\n%s\n}" % (name, page, setup, code)

def codegenerator(cls):
    yield "node [shape=box,fontsize=8]"
    for c in inspect.getmro(cls):
        yield '%s [label="%s\\n---\\n%s"]' % (
            c.__name__,c.__name__,get_attrs(c))
        bases = c.__bases__
        if bases: # generate edges parents -> child
            yield ''.join([' %s -> %s\n' % ( b.__name__,c.__name__)
                           for b in bases])
        if len(bases) > 1: # put all parents on the same level
            yield " {rank=same; %s}\n" % ''.join([
                '%s ' % b.__name__ for b in bases])

if __name__=="__main__":
    from Products.Archetypes.public import BaseFolder
    print dotcode(BaseFolder)

