from walk import walk, pprint
from operator import itemgetter
from itertools import groupby

nested_ls = [1,[2,[3,[[[4,5],6]]]],7]
pprint(nested_ls)

d = dict.fromkeys(range(6), 0)
levels = (lvl for obj, lvl in walk(nested_ls))

for lvl, grp in groupby(walk(nested_ls), key=itemgetter(1)):
    print lvl, list(grp)
