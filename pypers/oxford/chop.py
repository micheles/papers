# chop.py

# see also http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/303279

import itertools

def chop(iterable, batchsize):
    it = iter(iterable)
    while True:
        batch = list(itertools.islice(it, batchsize))
        if batch: yield batch
        else: break


