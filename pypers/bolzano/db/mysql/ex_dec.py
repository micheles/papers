def double(func):
    def wrapped_func(i):
        return 2*i
    return wrapped_func

def f(i):
    return i

f = double(f)

print f(5)
