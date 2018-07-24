"""General utilities involving iterables."""

import sets, itertools, os
try: # Python 2.4 vs. Python 2.3
    set
except NameError:
    from sets import Set as set

def check(it):
    """
    Checks if an iterator is empty. Returns a copy of the original iterator.
    
    >>> it = check(iter([1]))
    >>> if it: it.next()
    1
    """
    try:
        first = it.next()
    except StopIteration:
        return False
    else:
        return itertools.chain([first], it)
    
def skip_redundant(iterable, skipset=None):
   "Redundant items are repeated items or items in the original skipset."
   if skipset is None: skipset = set()
   for item in iterable:
       if item not in skipset:
           skipset.add(item)
           yield item
           
def chop(iterable, batchsize):
    """Chop an iterable. For instance
    
    >>> list(chop([1,2,3,4], 2))
    [[1, 2], [3, 4]]
    >>> list(chop([1,2,3,4,5,6,7],3))
    [[1, 2, 3], [4, 5, 6], [7]]
    
    It trunks the remainder elements, if the
    iterable is not divisible by batchsize.
    """
    it = iter(iterable)
    while True:
        batch = list(itertools.islice(it, batchsize))
        if batch: yield batch
        else: break
        
# used in the voting package
def first_duplicate_ls(it):
    """Returns None or a list with the duplicated element."""
    dupl = sets.Set()
    for el in it:
        if el in dupl:
            return [el]
        else:
            dupl.add(el)

# useful to display the list of votes in a short form
class PackedList(list):
    """Take a list with repetitions and pack it in the form

    PackedList([elements ..]) --> [(number_of_repetitions, element) ...]
    
    Gives a nice printing representation. Usage:
    PackedList(<list>, <string-repr-method> = str)
    It is possible to specify a custom string representation for
    the list elements."""
    
    def __init__(self, ls, to_str = str):
        self.to_str = to_str
        self.packedls = []
        self.pack(list(ls))
        self.extend(self.packedls)

    def pack(self, lst):
        """Recursive packer. At each call removes an element from ls and
        adds it to self.packedls. Returns when ls is fully depleted.
        """
        if not lst: return
        el, ls= lst[0], lst[1:]
        count = 1 # number of repetitions
        for i, elem in enumerate(ls[:]):
            if elem == el: # remove the duplicated element
                del ls[i+1-count] # in the right position
                count += 1
        self.packedls.append((count, el))
        self.pack(ls) # recurse until ls is empty

    def __str__(self):
        """Returns a table <number of repetions>: <element>"""
        return "\n".join(["%s: %s" % (t[0], self.to_str(t[1])) for t in self])

# a reiterable cycle going in both directions
class Cycle(object):
    def __init__(self, seq):
        self._list = list(seq)
        self._len = len(self._list)
        self.index = 0
    def __iter__(self):
        return itertools.cycle(self._list)
    def next(self):
        self.index += 1
        return self()
    def prev(self):
        self.index -= 1
        return self()
    def __len__(self):
        return self._len
    def __call__(self):
        return self._list[self.index % self._len]
    def __getitem__(self, i):
        return self._list[i % self._len]
    def __setitem__(self, i, v):
        self._list[i % self._len] = v
        
############ OLD VERSIONS ###############

## def chop_trunk(it, n = 2):
##     tup = (iter(it), ) * n
##     return itertools.izip(*tup)

## def chop_notrunk(iterable, binsize):
##     bin = []
##     for i, el in enumerate(iterable):
##         bin.append(el)
##         if i % binsize == binsize-1:
##             yield bin; bin = []
##     if bin:
##         yield bin

## def chop(iterable, binsize, trunk=False):
##     if trunk:
##         return chop_trunk(iterable, binsize)
##     else:
##         return chop_notrunk(iterable, binsize)

if __name__ == "__main__": # test Cycle
    c = Cycle([1,2,3])
    print c()
    print c.next()
    print c.next()
    print c.next()
    print c.next()
    print c.next()
    print c.prev()
    print c.prev()
    print c.prev()
    print c.prev()
    print c.prev()
    print c.prev()
