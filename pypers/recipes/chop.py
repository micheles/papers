# chop.py

def chop(iterable, n):
    bin = []
    for i, el in enumerate(iterable):
        bin.append(el)
        if i % n == n-1:
            yield bin; bin = []
    if bin:
        yield bin


