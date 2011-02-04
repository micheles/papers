class ListWrapper(object):
    def __init__(self, ls):
        self._list = ls
    def __getattr__(self, name):
        if name == "__getitem__":
            return self._list.__getitem__
        elif name == "reverse":
            return self._list.reverse
        else:
            return name

lw = ListWrapper([0,1,2])

print lw.x

lw.reverse()
print lw.__getitem__(0)
print lw.__getitem__(1)
print lw.__getitem__(2)
print lw[0]
