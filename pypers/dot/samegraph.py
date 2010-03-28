"Decide if two classes are in the same inheritance diagram."

import sets, inspect

def samegraph(A,B):
    "Returns true if the MRO's of A and B have an intersection."
    mro_A = sets.Set(inspect.getmro(A))
    mro_A.discard(object)
    mro_B = sets.Set(inspect.getmro(B))
    mro_B.discard(object)
    return bool(mro_A & mro_B)

def graphs(classes):
    "Given a list of classes, yields the disconnected graphs in the list."
    for a in classes:
        #print 'c',classes
        graph = [a]
        for b in classes[:]:
            if b is not a and samegraph(b,a):
                graph.append(b)
                classes.remove(b)
        yield graph
        
if __name__=='__main__': # test
    class A: pass
    class B(object): pass
    class C(B): pass
    assert samegraph(B,C)
    assert not samegraph(A,B)
    for graph in graphs([A,B,C]): print graph
