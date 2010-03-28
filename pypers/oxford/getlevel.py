def getlevel(obj, lvl, i):
    if lvl == 0:
        return obj[i]
    else:
        return getlevel(obj[i], lvl-1)

ls = [[0, [1, [2], 3], 4, 5]]

print getlevel(ls, 0, 1)
