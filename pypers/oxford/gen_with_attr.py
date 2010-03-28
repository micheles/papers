class MyIter(object):
    def __iter__(self):
        yield 1
        yield 2
        yield 3

it = MyIter()
for i in it: print i

i0 = MyIter()
i1 = iter(i0)
i2 = iter(i1)

print i0 is i1 
print i1 is i2 

it = MyIter()
print it, iter(it)
print iter(it), iter(iter(it))


