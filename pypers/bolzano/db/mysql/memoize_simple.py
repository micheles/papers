import time

## def memoize(func):
##     return func

memoize_dic = {}

def create_graph(n):
    if n in memoize_dic:
        return memoize_dic[n]
    time.sleep(3)
    result = "graph%s" % n
    memoize_dic[n] = result
    return result

print "created ", create_graph(1)   
print "created ", create_graph(1)
 
