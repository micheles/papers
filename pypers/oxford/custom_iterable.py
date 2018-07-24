# custom_iterable.py
class MyIter(object):
    def __iter__(self):
        yield 1
        yield 2
        yield 3

it = MyIter(); # print it, iter(it)

