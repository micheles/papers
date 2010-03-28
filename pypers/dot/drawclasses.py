"""Given a Python module or package, it draws the classes defined in it.
It is smart enough to recognize disconnected inheritance hierarchies. 
Usage:

$ python drawclasses.py <module-or-package-in-the-pythonpath>
"""

import os, types, inspect
from ms.tools import minidoc
from ms.iter_utils import skip_redundant as remove_dupl
    
def ismeta(c):
    return isinstance(c, type) and issubclass(c, type)

def isclass(c):
    return isinstance(c, (type, types.ClassType))
    
def getclasses(module_or_package):
    x = minidoc.import_(module_or_package)
    if hasattr(x, "__path__"): # is package
        packagecontent = os.listdir(x.__path__[0])
        for fname in packagecontent: 
            if fname.endswith('.py') or fname.endswith('.pyc') \
                   or fname.endswith('.pyd') or fname.endswith('.pyo'):
                module = x.__name__ + "." + minidoc.body(fname)
                for cls in getclasses(module):
                    yield cls
    else: # is module
        for name,obj in x.__dict__.iteritems(): 
            if isclass(obj):
                yield obj

def codegen(name, ls):
    drawn = set()
    yield 'digraph %s{' % name
    # yield 'caption [shape=box,label=%s\n]' % name
    yield 'size="7,10"'
    # yield 'rotate=90'
    for c in ls:
        bases = list(c.__bases__)
        if object in bases:
            bases.remove(object)
        for b in bases:
            yield "%s-> %s" % (b.__name__,c.__name__)
            drawn.add(b)
        #yield "%s -> %s [style=dashed]" % (c.__name__,type(c).__name__)
        drawn.add(c)
    yield ''
    for c in drawn:
        if ismeta(c):
            name = "%s" % c.__name__
        else:
            name = "%s [shape=box]" % c.__name__
        if hasattr(c,'__metaclass__'):
            name += ' [color=red]' # ' [style=dashed]'
        yield name
    yield '}'

def mro(cls):
    return inspect.getmro(cls)
        
def samegraph(A, B): # always True for new-style
    "Returns true if the MRO's of A and B have an intersection."
    mro_A = set(mro(A))
    mro_B = set(mro(B))
    return bool(mro_A & mro_B)

def graphs(classes):
    "Given a set of classes, yields the connected subsets."
    klasses = list(classes)
    for a in klasses:
        graph = [a]
        for b in klasses[:]:
            if b is not a and samegraph(b,a):
                graph.append(b)
                klasses.remove(b)
        yield graph

def graphgen(classes):
    """Yields inheritance graphs."""
    i = 1; singles = [] # single classes
    for graph in graphs(classes):
        if len(graph) > 1: # it's a hierarchy
            name = '"Diagram #%s"' % i; i += 1
            yield '\n'.join(remove_dupl(codegen(name, graph)))
        else: # it's a single class (may derive from a built-in)
            singles.append(graph[0])
    if singles:
        name = '"Diagram #%s"' % i
        yield '\n'.join(remove_dupl(codegen(name, singles)))

if __name__ == '__main__':
    import sys
    try:
        package = sys.argv[1]
    except:
        sys.exit(__doc__)
    classes = list(getclasses(package))
    for code in graphgen(classes):
        #os.popen('dot -Tps -o /tmp/x.ps; evince /tmp/x.ps','w').write(code)
        os.popen('dot -Tpng -o /tmp/x.png; firefox /tmp/x.png','w').write(code)
        print code
