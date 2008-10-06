# tabular_data.py
from namedtuple import namedtuple

def headtail(iterable):
    "Returns the head and the tail of a non-empty iterator"
    it = iter(iterable)
    return it.next(), it

def get_table(header_plus_body, ntuple=None):
    "Return a sequence of namedtuples in the form header+body"
    header, body = headtail(header_plus_body)
    ntuple = ntuple or namedtuple('NamedTuple', header)
    yield ntuple(*header)
    for row in body:
        yield ntuple(*row)

def test1():
    "Read the fields from the first row"
    data = [['title', 'author'], ['Records in Python', 'M. Simionato']]
    for nt in get_table(data):
        print nt

def test2():
    "Use a predefined namedtuple class"
    data = [['title', 'author'], ['Records in Python', 'M. Simionato']]
    for nt in get_table(data, namedtuple('NamedTuple', 'tit auth')):
        print nt

if __name__ == '__main__':
    test1()
    test2()
