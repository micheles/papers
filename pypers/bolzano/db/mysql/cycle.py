#from itertools import cycle

class CycleOld(object):
    def __init__(self, seq):
        self.seq = list(seq)
        self.index = 0
        self.min_index = 0
        self.max_index = len(self.seq) - 1
    def __call__(self):
        return self.seq[self.index]
    def prev(self):
        if self.index == self.min_index:
            self.index = self.max_index
        else:
            self.index -= 1
        return self()
    def next(self):
        if self.index == self.max_index:
            self.index = self.min_index
        else:
            self.index += 1
        return self()

class Cycle(object):
    def __init__(self, seq):
        self.seq = list(seq)
        self.len = len(self.seq)
        self.index = 0
    def __call__(self):
        return self.seq[self.index % self.len]
    def prev(self):
        self.index -= 1
        return self()
    def next(self):
        self.index += 1
        return self()

def chop(seq, binsize):
    bin = []
    for i, el in enumerate(seq):
        bin.append(el)
        if i % binsize == binsize - 1:
            yield bin; bin = []
    if bin:
        yield bin

if __name__ == "__main__":
    print list(chop("precipitevolissimevolmente", 3))
        
# chop([1,2,3,4,5,6], 2)
# [[1,2], [3,4], [4.6]]


## cycle = Cycle2([1,2,3])
## print cycle()
## print cycle.prev()
## print cycle.prev()
## print cycle.prev()
## print cycle.prev()
## print "-"*10
## print cycle.next()
## print cycle.next()
## print cycle.next()
## print cycle.next()
        
        
