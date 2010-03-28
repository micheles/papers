import time

memoize_dic = {}

# memoize decorator
def memoize(func):
    def wrapped_func(*args):
        if args in memoize_dic:
            return memoize_dic[args]
        else:
            result = func(*args)
            memoize_dic[args] = result
            return result
    wrapped_func.__name__ = func.__name__
    return wrapped_func

@memoize
def create_graph(n):
    time.sleep(3)
    return "graph%s" % n

