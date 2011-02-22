from ms.file_utils import ifiles
import os

def gen(fname):
    for line in file(fname):
        yield line

def multi_iter(iters):
    iters = list(iters) # to avoid side effects
    while iters:
        for it in iters:
            try:
                yield it.next()
            except StopIteration:
                iters.remove(it)

ci =multi_iter(gen(f) for f in
         ifiles("/home/micheles/md/python/Timing",
                lambda f: f.endswith(".txt")))

for line in ci:
    print line,
